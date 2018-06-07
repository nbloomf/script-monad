{- |
Module      : Control.Monad.Script.Http
Description : A generic monad for expressing HTTP interactions.
Copyright   : 2018, Automattic, Inc.
License     : BSD3
Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
Stability   : experimental
Portability : POSIX

A basic type and monad for describing HTTP interactions.
-}

{-# LANGUAGE GADTs, Rank2Types, RecordWildCards #-}
module Control.Monad.Script.Http (
  -- * Http
    Http()
  , execHttpM

  -- * Error
  , catch

  , E(..)
  , JsonError(..)
  , printE

  -- * Reader
  , ask
  , local
  , reader

  , R(..)
  , LogOptions(..)
  , basicLogOptions
  , basicEnv

  -- * Writer
  , W()
  , logEntries

  -- * State
  , modify
  , gets

  , S(..)
  , basicState

  -- * Prompt
  , prompt

  , P(..)
  , Url
  , HttpResponse(..)
  , evalIO

  -- * API
  , comment
  , wait
  , log

  , throwError
  , throwJsonError

  , httpGet
  , httpSilentGet
  , httpPost
  , httpSilentPost
  , httpDelete
  , httpSilentDelete

  , parseJson
  , lookupKeyJson
  , constructFromJson

  -- * Shell
  , initShell
  , httpShell
) where

import Prelude hiding (lookup, log)

import Control.Applicative
  ( Applicative(..), (<$>) )
import Control.Concurrent
  ( threadDelay )
import Control.Concurrent.MVar
  ( MVar, withMVar )
import Control.Exception
  ( IOException, Exception, try )
import Control.Monad
  ( Functor(..), Monad(..), ap )
import Control.Lens
  ( preview, (^.) )
import Data.Aeson
  ( Value(Object), Result(Success,Error), FromJSON, fromJSON, decode )
import Data.Aeson.Encode.Pretty
  ( encodePretty )
import Data.Aeson.Lens
  ( _Value )
import Data.ByteString.Lazy
  ( ByteString, fromStrict, readFile, writeFile )
import Data.ByteString.Lazy.Char8
  ( unpack, pack )
import Data.HashMap.Strict
  ( lookup )
import Data.IORef
  ( IORef, newIORef, readIORef, writeIORef )
import Data.Text
  ( Text )
import Data.Time
  ( UTCTime )
import Data.Time.Clock.System
  ( getSystemTime, systemToUTCTime )
import Data.Typeable
  ( Typeable )
import Data.Monoid
  ( Monoid(..) )
import Network.HTTP.Client
  ( HttpException(..), CookieJar, HttpExceptionContent(StatusCodeException)
  , Response, responseCookieJar, responseBody
  , responseHeaders, responseVersion, responseStatus )
import Network.HTTP.Types
  ( HttpVersion, Status, ResponseHeaders )
import qualified Network.Wreq as Wreq
  ( Options, getWith, postWith, deleteWith, defaults, responseStatus )
import qualified Network.Wreq.Session as S
  ( Session, newSession, getWith, postWith, deleteWith )
import System.IO
  ( Handle, hPutStrLn, hGetEcho, hSetEcho, hFlush
  , hFlush, hGetLine, hPutStr, hPutChar, stdout )
import System.IO.Error
  ( ioeGetFileName, ioeGetLocation, ioeGetErrorString )

import qualified Control.Monad.Script as S





-- | An HTTP session returning an @a@, writing to a log of type @W e w@, reading from an environment of type @R e w r@, with state of type @S s@, throwing errors of type @E e@, and performing effectful computations described by @P p a@.
--
-- Behind the scenes @Http@ is a stack of reader, writer, state, error, and prompt monads.
newtype Http e r w s p a = Http
  { http :: S.Script (E e) (R e w r) (W e w) (S s) (P p) a
  } deriving Typeable

-- | Execute an HTTP session.
execHttpM
  :: (Monad m)
  => S s -- ^ Initial state
  -> R e w r -- ^ Environment
  -> (forall a. P p a -> m a) -- ^ Effect evaluator
  -> Http e r w s p t
  -> m (Either (E e) t, S s, W e w)
execHttpM s r p = S.execScriptM s r p . http

instance Functor (Http e r w s p) where
  fmap f = Http . fmap f . http

instance Applicative (Http e r w s p) where
  pure = return
  (<*>) = ap

instance Monad (Http e r w s p) where
  return = Http . return
  (Http x) >>= f = Http (x >>= (http . f))

ask
  :: Http e r w s p (R e w r)
ask = Http S.ask

local
  :: (R e w r -> R e w r)
  -> Http e r w s p a
  -> Http e r w s p a
local f = Http . S.local f . http

transport
  :: (R e w r2 -> R e w r1)
  -> Http e r1 w s p a
  -> Http e r2 w s p a
transport f = Http . S.transport f . http

reader
  :: (R e w r -> a)
  -> Http e r w s p a
reader f = Http (S.reader f)

get
  :: Http e r w s p (S s)
get = Http S.get

put
  :: S s
  -> Http e r w s p ()
put s = Http (S.put s)

modify
  :: (S s -> S s)
  -> Http e r w s p ()
modify f = Http (S.modify' f)

gets
  :: (S s -> a)
  -> Http e r w s p a
gets f = Http (S.gets f)

-- | Do not export; we want to only allow writes to the log via functions that call `logNow`.
tell
  :: W e w
  -> Http e r w s p ()
tell w = Http (S.tell w)

pass
  :: Http e r w s p (a, W e w -> W e w)
  -> Http e r w s p a
pass = Http . S.pass . http

censor
  :: (W e w -> W e w)
  -> Http e r w s p a
  -> Http e r w s p a
censor f = Http . S.censor f . http

except
  :: Either (E e) a
  -> Http e r w s p a
except e = Http (S.except e)

throw
  :: E e
  -> Http e r w s p a
throw e = Http (S.throw e)

catch
  :: Http e r w s p a
  -> (E e -> Http e r w s p a)
  -> Http e r w s p a
catch x f = Http (S.catch (http x) (http . f))

prompt
  :: P p a
  -> Http e r w s p a
prompt p = Http (S.prompt p)





-- | Error type.
data E e
  = E_Http HttpException
  | E_IO IOException
  | E_Json JsonError
  | E e -- ^ Client-supplied error type.
  deriving Show

-- | Represents the kinds of errors that can occur when parsing and decoding JSON.
data JsonError
  = JsonError -- ^ A generic JSON error; try not to use this.
  | JsonParseError -- ^ A failed parse.
  | JsonKeyDoesNotExist Text -- ^ An attempt to look up the value of a key that does not exist on an object.
  | JsonKeyLookupOffObject Text -- ^ An attempt to look up the value of a key on something other than an object.
  | JsonConstructError String -- ^ A failed attempt to convert a `Value` to some other type.
  deriving (Eq, Show)

-- | Pretty printer for errors.
printE
  :: (e -> String) -- ^ Pretty printer for client-supplied error type.
  -> E e
  -> String
printE f e = case e of
  E_Http err -> show err
  E_IO err -> show err
  E_Json err -> show err
  E err -> f err





-- | Generic session environment.
data R e w r = R
  { _logOptions :: LogOptions e w

  , _logHandle :: Handle

  -- | Lock used to prevent race conditions when writing to the log.
  , _logLock :: MVar ()

  , _uid :: String

  -- | Function for elevating 'HttpException's to a client-supplied error type.
  , _httpErrorInject :: HttpException -> Maybe e 

  -- | Client-supplied environment type.
  , _userEnv :: r
  }

-- | Options for tweaking the logs.
data LogOptions e w = LogOptions
  { -- | Toggle color
    _logColor :: Bool

    -- | Toggle JSON pretty printing
  , _logJson :: Bool

    -- | Toggle to silence the logs
  , _logSilent :: Bool

    -- | Printer for client-supplied error type. The boolean toggles JSON pretty printing.
  , _printUserError :: Bool -> e -> String

    -- | Printer for client-supplied log type. the boolean toggles JSON pretty printing.
  , _printUserLog :: Bool -> w -> String
  }

basicLogOptions :: (Show e, Show w) => LogOptions e w
basicLogOptions = LogOptions
  { _logColor = True
  , _logJson = False
  , _logSilent = False
  , _printUserError = \_ e -> show e
  , _printUserLog = \_ w -> show w
  }

-- | Environment constructor
basicEnv
  :: (Show e, Show w)
  => MVar ()
  -- ^ Lock; used to prevent race conditions when writing to the log.
  -> r
  -- ^ Client-supplied environment value.
  -> R e w r
basicEnv lock r = R
  { _httpErrorInject = const Nothing
  , _logOptions = basicLogOptions
  , _logHandle = stdout
  , _logLock = lock
  , _uid = ""
  , _userEnv = r
  }





-- | Log type
data W e w = W [(UTCTime, Log e w)]

instance Monoid (W e w) where
  mempty = W []
  mappend (W a1) (W a2) = W (a1 ++ a2)

-- | Log entry type
data Log e w
  = L_Comment String
  | L_Request HttpVerb Url Wreq.Options (Maybe ByteString)
  | L_SilentRequest
  | L_Response HttpResponse
  | L_SilentResponse
  | L_Pause Int
  | L_HttpError HttpException
  | L_IOError IOException
  | L_JsonError JsonError

  -- | Client-supplied error type
  | L_Error e

  -- | Client-supplied log entry type
  | L_Log w
  deriving Show

type Url = String

-- | Used in the logs.
data HttpVerb
  = DELETE | GET | POST
  deriving (Eq, Show)

data HttpResponse = HttpResponse
  { _responseStatus :: Status
  , _responseVersion :: HttpVersion
  , _responseHeaders :: ResponseHeaders
  , _responseBody :: ByteString
  , _responseCookieJar :: CookieJar
  } deriving (Eq, Show)

-- | Convert errors to log entries
errorMessage :: E e -> Log e w
errorMessage e = case e of
  E_Http err -> L_HttpError err
  E_IO err -> L_IOError err
  E_Json err -> L_JsonError err
  E e -> L_Error e

-- | Used to specify colors for user-supplied log entries.
data Color
  = Red | Blue | Green | Yellow | Magenta

inColor :: Color -> String -> String
inColor c msg = case c of
  Red -> "\x1b[1;31m" ++ msg ++ "\x1b[0;39;49m"
  Blue -> "\x1b[1;34m" ++ msg ++ "\x1b[0;39;49m"
  Green -> "\x1b[1;32m" ++ msg ++ "\x1b[0;39;49m"
  Yellow -> "\x1b[1;33m" ++ msg ++ "\x1b[0;39;49m"
  Magenta -> "\x1b[1;35m" ++ msg ++ "\x1b[0;39;49m"

printEntryWith
  :: Bool
  -> (Bool -> e -> String)
  -> (Bool -> w -> String)
  -> Log e w
  -> (Color, String)
printEntryWith asJson printError printLog entry = case entry of
  L_Comment msg -> (Green, msg)

  L_Request verb url opt payload -> case payload of
    Just p -> if asJson
      then
        let
          json = case decode p of
             Nothing -> "parse error:\n" ++ unpack p
             Just v -> ('\n':) $ unpack $ encodePretty (v :: Value)
         in (Blue, unlines [unwords [show verb, url], json])
      else (Blue, unlines [unwords [show verb, url], unpack p])

    _ -> (Blue, unwords [show verb, url])

  L_SilentRequest -> (Blue, "Silent Request")

  L_Response response -> if asJson
    then
      let
        headers = _responseHeaders response
        json = unpack $ encodePretty $ preview _Value $ _responseBody response
      in (Blue, unlines ["Response", show headers, json])
    else (Blue, show response)

  L_SilentResponse -> (Blue, "Silent Response")

  L_Pause k -> (Magenta, "Wait for " ++ show k ++ "μs")

  L_HttpError e -> if asJson
    then
      let
        unpackHttpError :: HttpException -> Maybe (String, String)
        unpackHttpError err = case err of
          HttpExceptionRequest _ (StatusCodeException s r) -> do
            json <- decode $ fromStrict r
            let status = s ^. Wreq.responseStatus 
            return (show status, unpack $ encodePretty (json :: Value))
          _ -> Nothing
      in
        case unpackHttpError e of
          Nothing -> (Red, show e)
          Just (code, json) -> (Red, unlines [ unwords [ "HTTP Error Response", code], json ])

    else (Red, show e)

  L_IOError e -> (Red, unwords [ show $ ioeGetFileName e, ioeGetLocation e, ioeGetErrorString e ])

  L_JsonError e -> (Red, "JSON Error: " ++ show e)

  L_Error e -> (Red, unwords [ "ERROR", printError asJson e ])

  L_Log w -> (Yellow, unwords [ "INFO", printLog asJson w ])

-- | Render a log entry
printLogWith :: LogOptions e w -> (UTCTime, String, Log e w) -> Maybe String
printLogWith opt@LogOptions{..} (timestamp, uid, entry) = do
  if _logSilent
    then Nothing
    else do
      let
        time :: String
        time = take 19 $ show timestamp

        color :: Color -> String -> String
        color c = if _logColor then inColor c else id

        (c,msg) = printEntryWith _logJson _printUserError _printUserLog entry

      Just $ unwords $ filter (/= "")
        [ color c time, uid, msg ]

logEntries :: W e w -> [w]
logEntries (W xs) = entries xs
  where
    entries [] = []
    entries ((_,w):ws) = case w of
      L_Log u -> u : entries ws
      _ -> entries ws




-- | State type
data S s = S
  { _httpOptions :: Wreq.Options
  , _httpSession :: Maybe S.Session
  , _userState :: s
  }

-- | State constructor
basicState :: s -> S s
basicState s = S
  { _httpOptions = Wreq.defaults
  , _httpSession = Nothing
  , _userState = s
  }





-- | Atomic effects
data P p a where
  HPutStrLn :: Handle -> String -> P p ()
  HPutStrLnBlocking :: MVar () -> Handle -> String -> P p ()

  GetSystemTime :: P p UTCTime
  ThreadDelay :: Int -> P p ()

  HttpGet
    :: Wreq.Options -> Maybe S.Session -> Url
    -> P p (Either HttpException HttpResponse)
  HttpPost
    :: Wreq.Options -> Maybe S.Session -> Url
    -> ByteString -> P p (Either HttpException HttpResponse)
  HttpDelete
    :: Wreq.Options -> Maybe S.Session -> Url
    -> P p (Either HttpException HttpResponse)

  P :: p a -> P p a

-- | Basic evaluator for interpreting atomic 'Http' effects in 'IO'.
evalIO
  :: (p a -> IO a) -- ^ Evaluator for user effects
  -> P p a
  -> IO a
evalIO eval x = case x of
  HPutStrLn handle string -> do
    hPutStrLn handle string
    hFlush handle

  HPutStrLnBlocking lock handle str -> do
    withMVar lock (\() -> hPutStrLn handle str)
    hFlush handle

  GetSystemTime -> fmap systemToUTCTime getSystemTime

  ThreadDelay k -> threadDelay k

  HttpGet opts s url -> case s of
    Nothing -> try $ readHttpResponse <$> Wreq.getWith opts url
    Just sn -> try $ readHttpResponse <$> S.getWith opts sn url

  HttpPost opts s url msg -> case s of
    Nothing -> try $ readHttpResponse <$> Wreq.postWith opts url msg
    Just sn -> try $ readHttpResponse <$> S.postWith opts sn url msg

  HttpDelete opts s url -> case s of
    Nothing -> try $ readHttpResponse <$> Wreq.deleteWith opts url
    Just sn -> try $ readHttpResponse <$> S.deleteWith opts sn url

  P act -> eval act

-- | Convert an opaque `Response ByteString` into an `HttpResponse`.
readHttpResponse :: Response ByteString -> HttpResponse
readHttpResponse r = HttpResponse
  { _responseStatus = responseStatus r
  , _responseVersion = responseVersion r
  , _responseHeaders = responseHeaders r
  , _responseBody = responseBody r
  , _responseCookieJar = responseCookieJar r
  }





logNow
  :: Log e w
  -> Http e r w s p ()
logNow msg = do
  time <- prompt $ GetSystemTime
  R{..} <- ask
  case printLogWith _logOptions (time,_uid,msg) of
    Nothing -> return ()
    Just str -> prompt $ HPutStrLnBlocking _logLock _logHandle str
  tell $ W [(time, msg)]

comment
  :: String
  -> Http e r w s p ()
comment msg = do
  logNow $ L_Comment msg

wait
  :: Int
  -> Http e r w s p ()
wait k = do
  logNow $ L_Pause k
  prompt $ ThreadDelay k

log
  :: w
  -> Http e r w s p ()
log = logNow . L_Log

throwError
  :: E e
  -> Http e r w s p a
throwError e = do
  logNow $ errorMessage e
  throw e

throwJsonError
  :: JsonError
  -> Http e r w s p a
throwJsonError e = do
  logNow $ errorMessage $ E_Json e
  throw $ E_Json e

httpGet
  :: Url
  -> Http e r w s p HttpResponse
httpGet url = do
  R{..} <- ask
  S{..} <- get
  logNow $ L_Request GET url _httpOptions Nothing
  result <- prompt $ HttpGet _httpOptions _httpSession url
  case result of
    Right response -> do
      logNow $ L_Response response
      return response
    Left err -> case _httpErrorInject err of
      Just z -> throwError $ E z
      Nothing -> throwError $ E_Http err

httpSilentGet
  :: Url
  -> Http e r w s p HttpResponse
httpSilentGet url = do
  R{..} <- ask
  S{..} <- get
  logNow $ L_SilentRequest
  result <- prompt $ HttpGet _httpOptions _httpSession url
  case result of
    Right response -> do
      logNow $ L_SilentResponse
      return response
    Left err -> case _httpErrorInject err of
      Just z -> throwError $ E z
      Nothing -> throwError $ E_Http err

httpPost
  :: Url
  -> ByteString
  -> Http e r w s p HttpResponse
httpPost url payload = do
  R{..} <- ask
  S{..} <- get
  logNow $ L_Request POST url _httpOptions (Just payload)
  result <- prompt $ HttpPost _httpOptions _httpSession url payload
  case result of
    Right response -> do
      logNow $ L_Response response
      return response
    Left err -> case _httpErrorInject err of
      Just z -> throwError $ E z
      Nothing -> throwError $ E_Http err

httpSilentPost
  :: Url
  -> ByteString
  -> Http e r w s p HttpResponse
httpSilentPost url payload = do
  R{..} <- ask
  S{..} <- get
  logNow $ L_SilentRequest
  result <- prompt $ HttpPost _httpOptions _httpSession url payload
  case result of
    Right response -> do
      logNow $ L_SilentResponse
      return response
    Left err -> case _httpErrorInject err of
      Just z -> throwError $ E z
      Nothing -> throwError $ E_Http err

httpDelete
  :: Url
  -> Http e r w s p HttpResponse
httpDelete url = do
  R{..} <- ask
  S{..} <- get
  logNow $ L_Request DELETE url _httpOptions Nothing
  result <- prompt $ HttpDelete _httpOptions _httpSession url
  case result of
    Right response -> do
      logNow $ L_Response response
      return response
    Left err -> case _httpErrorInject err of
      Just z -> throwError $ E z
      Nothing -> throwError $ E_Http err

httpSilentDelete
  :: Url
  -> Http e r w s p HttpResponse
httpSilentDelete url = do
  R{..} <- ask
  S{..} <- get
  logNow $ L_SilentRequest
  result <- prompt $ HttpDelete _httpOptions _httpSession url
  case result of
    Right response -> do
      logNow $ L_SilentResponse
      return response
    Left err -> case _httpErrorInject err of
      Just z -> throwError $ E z
      Nothing -> throwError $ E_Http err

-- | Decode a `ByteString` to an `Value`.
parseJson :: ByteString -> Http e r w s p Value
parseJson bytes = case preview _Value bytes of
  Just value -> return value
  Nothing -> throwError $ E_Json JsonParseError

-- | Object member lookup.
lookupKeyJson :: Text -> Value -> Http e r w s p Value
lookupKeyJson key (Object obj) = case lookup key obj of
  Nothing -> throwError $ E_Json (JsonKeyDoesNotExist key)
  Just value -> return value
lookupKey key _ = throwError $ E_Json (JsonKeyLookupOffObject key)

-- | Decode a `A.Value` to some other type.
constructFromJson :: (FromJSON a) => Value -> Http e r w s p a
constructFromJson value = case fromJSON value of
  Success x -> return x
  Error msg -> throwError $ E_Json (JsonConstructError msg)





-- | Initialize a context for running an HTTP shell interaction.
initShell
  :: S s
  -> R e w r
  -> IO (IORef (S s, R e w r))
initShell s r =
  newIORef (s,r)

-- | Execute an `Http` action in the shell.
httpShell
  :: (Show e)
  => IORef (S s, R e w r)
  -> (forall a. p a -> IO a)
  -> Http e r w s p a
  -> IO a
httpShell ref eval session = do
  (st1,env) <- readIORef ref
  (result, st2, _) <- execHttpM st1 env (evalIO eval) session
  writeIORef ref (st2,env)
  case result of
    Left err -> error $ printE show err
    Right ok -> return ok
