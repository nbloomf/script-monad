{- |
Module      : Control.Monad.Script.Http
Description : A generic monad for expressing HTTP interactions.
Copyright   : 2018, Automattic, Inc.
License     : BSD3
Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
Stability   : experimental
Portability : POSIX

A basic type and monad transformer transformer for describing HTTP interactions.
-}

{-#
  LANGUAGE
    GADTs,
    Rank2Types,
    RecordWildCards,
    QuantifiedConstraints
#-}

module Control.Monad.Script.Http (
  -- * HttpT
    HttpT()

  -- * HttpT
  , HttpTT()
  , execHttpTT
  , liftHttpTT

  -- * Error
  , throwError
  , throwJsonError
  , throwHttpException
  , throwIOException
  , catchError
  , catchJsonError
  , catchHttpException
  , catchIOException
  , catchAnyError
  , printError
  , E()

  -- * Reader
  , ask
  , local
  , reader
  , R(..)
  , basicEnv
  , trivialEnv
  , LogOptions(..)
  , basicLogOptions
  , trivialLogOptions

  -- * Writer
  , logEntries
  , LogSeverity(..)
  , setLogSeverity
  , W()
  , printHttpLogs
  , basicLogEntryPrinter

  -- * State
  , gets
  , modify
  , S(..)
  , basicState

  -- * Prompt
  , prompt
  , P(..)
  , evalIO
  , evalMockIO

  -- * API
  , comment
  , wait
  , logDebug
  , logInfo
  , logNotice
  , logWarning
  , logError
  , logCritical
  , logAlert
  , logEmergency

  -- ** IO
  , Control.Monad.Script.Http.hPutStrLn
  , hPutStrLnBlocking

  -- ** HTTP calls
  , httpGet
  , httpSilentGet
  , httpPost
  , httpSilentPost
  , httpDelete
  , httpSilentDelete

  -- ** JSON
  , parseJson
  , lookupKeyJson
  , constructFromJson

  -- * Types
  , Url
  , JsonError(..)
  , HttpResponse(..)

  -- * Testing
  , checkHttpTT
) where

import Prelude hiding (lookup)

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
import Control.Monad.Trans.Class
  ( MonadTrans(..) )
import Control.Monad.Trans.Identity
  ( IdentityT(..) )
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
import Data.Functor.Identity
  ( Identity() )
import Data.HashMap.Strict
  ( lookup )
import Data.IORef
  ( IORef, newIORef, readIORef, writeIORef )
import Data.List
  ( intercalate )
import Data.String
  ( fromString )
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
import Data.Semigroup
  ( Semigroup(..) )
import Network.HTTP.Client
  ( HttpException(..), CookieJar, HttpExceptionContent(StatusCodeException)
  , Response, responseCookieJar, responseBody
  , responseHeaders, responseVersion, responseStatus )
import Network.HTTP.Types
  ( HttpVersion, Status, ResponseHeaders )
import qualified Network.Wreq as Wreq
  ( Options, getWith, postWith, deleteWith, defaults, responseStatus, headers )
import qualified Network.Wreq.Session as S
  ( Session, newSession, getWith, postWith, deleteWith )
import System.IO
  ( Handle, hPutStrLn, hGetEcho, hSetEcho, hFlush
  , hFlush, hGetLine, hPutStr, hPutChar, stdout )
import System.IO.Error
  ( ioeGetFileName, ioeGetLocation, ioeGetErrorString )
import Test.QuickCheck
  ( Property, Arbitrary(..), Gen )

import qualified Control.Monad.Script as S
import Network.HTTP.Client.Extras
import Data.Aeson.Extras
import Data.LogSeverity
import Data.MockIO
import Data.MockIO.FileSystem



-- | An HTTP session returning an @a@, writing to a log of type @W e w@, reading from an environment of type @R e w r@, with state of type @S s@, throwing errors of type @E e@, performing effectful computations described by @P p a@, and with inner monad @t eff@.
newtype HttpTT e r w s p t eff a = HttpTT
  { httpTT :: S.ScriptTT (E e) (R e w r) (W e w) (S s) (P p) t eff a
  } deriving Typeable

-- | An HTTP session returning an @a@, writing to a log of type @W e w@, reading from an environment of type @R e w r@, with state of type @S s@, throwing errors of type @E e@, performing effectful computations described by @P p a@, with inner monad @eff@. `HttpTT` over `IdentityT`.
type HttpT e r w s p = HttpTT e r w s p IdentityT

instance
  (Monad eff, Monad (t eff), MonadTrans t)
    => Functor (HttpTT e r w s p t eff) where
  fmap f = HttpTT . fmap f . httpTT

instance
  (Monad eff, Monad (t eff), MonadTrans t)
    => Applicative (HttpTT e r w s p t eff) where
  pure = return
  (<*>) = ap

instance
  (Monad eff, Monad (t eff), MonadTrans t)
    => Monad (HttpTT e r w s p t eff) where
  return = HttpTT . return
  (HttpTT x) >>= f = HttpTT (x >>= (httpTT . f))

instance
  (MonadTrans t, forall m. (Monad m) => Monad (t m))
    => MonadTrans (HttpTT e r w s p t) where
  lift = HttpTT . lift

liftHttpTT
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => t eff a -> HttpTT e r w s p t eff a
liftHttpTT = HttpTT . S.liftScriptTT





-- | Execute an `HttpTT` session.
execHttpTT
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => S s -- ^ Initial state
  -> R e w r -- ^ Environment
  -> (forall u. P p u -> eff u) -- ^ Effect evaluator
  -> HttpTT e r w s p t eff a
  -> t eff (Either (E e) a, S s, W e w)
execHttpTT s r p = S.execScriptTT s r p . httpTT

-- | Turn an `HttpTT` into a property; for testing with QuickCheck.
checkHttpTT
  :: (Monad eff, Monad (t eff), MonadTrans t, Show q)
  => S s -- ^ Initial state
  -> R e w r -- ^ Environment
  -> (forall u. P p u -> eff u) -- ^ Effect evaluator
  -> (t eff (Either (E e) a, S s, W e w) -> IO q) -- ^ Condense to `IO`
  -> (q -> Bool) -- ^ Result check
  -> HttpTT e r w s p t eff a
  -> Property
checkHttpTT s r eval cond check =
  S.checkScriptTT s r eval cond check . httpTT



-- | Retrieve the environment.
ask
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => HttpTT e r w s p t eff (R e w r)
ask = HttpTT S.ask

-- | Run an action with a locally adjusted environment of the same type.
local
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => (R e w r -> R e w r)
  -> HttpTT e r w s p t eff a
  -> HttpTT e r w s p t eff a
local f = HttpTT . S.local f . httpTT

-- | Run an action with a locally adjusted environment of a possibly different type.
transport
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => (R e w r2 -> R e w r1)
  -> HttpTT e r1 w s p t eff a
  -> HttpTT e r2 w s p t eff a
transport f = HttpTT . S.transport f . httpTT

-- | Retrieve the image of the environment under a given function.
reader
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => (R e w r -> a)
  -> HttpTT e r w s p t eff a
reader f = HttpTT (S.reader f)

-- | Retrieve the current state.
get
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => HttpTT e r w s p t eff (S s)
get = HttpTT S.get

-- | Replace the state.
put
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => S s
  -> HttpTT e r w s p t eff ()
put s = HttpTT (S.put s)

-- | Modify the current state strictly.
modify
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => (S s -> S s)
  -> HttpTT e r w s p t eff ()
modify f = HttpTT (S.modify' f)

-- | Retrieve the image of the current state under a given function.
gets
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => (S s -> a)
  -> HttpTT e r w s p t eff a
gets f = HttpTT (S.gets f)

-- | Do not export; we want to only allow writes to the log via functions that call @logNow@.
tell
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => W e w
  -> HttpTT e r w s p t eff ()
tell w = HttpTT (S.tell w)

-- | Run an action that returns a value and a log-adjusting function, and apply the function to the local log.
pass
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => HttpTT e r w s p t eff (a, W e w -> W e w)
  -> HttpTT e r w s p t eff a
pass = HttpTT . S.pass . httpTT

-- | Run an action, applying a function to the local log.
censor
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => (W e w -> W e w)
  -> HttpTT e r w s p t eff a
  -> HttpTT e r w s p t eff a
censor f = HttpTT . S.censor f . httpTT

-- | Inject an 'Either' into a 'Script'.
except
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => Either (E e) a
  -> HttpTT e r w s p t eff a
except e = HttpTT (S.except e)

-- | Raise an error
throw
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => E e
  -> HttpTT e r w s p t eff a
throw e = HttpTT (S.throw e)

-- | Run an action, applying a handler in case of an error result.
catch
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => HttpTT e r w s p t eff a -- ^ Computation that may raise an error
  -> (E e -> HttpTT e r w s p t eff a) -- ^ Handler
  -> HttpTT e r w s p t eff a
catch x f = HttpTT (S.catch (httpTT x) (httpTT . f))

-- | Inject an atomic effect.
prompt
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => P p a
  -> HttpTT e r w s p t eff a
prompt p = HttpTT (S.prompt p)



-- | Error type.
data E e
  = E_Http HttpException
  | E_IO IOException
  | E_Json JsonError
  | E e -- ^ Client-supplied error type.
  deriving Show

-- | Pretty printer for errors
printError :: (e -> String) -> E e -> String
printError p err = case err of
  E_Http e -> unlines [ "HTTP Exception:", show e ]
  E_IO e -> unlines [ "IO Exception:", show e ]
  E_Json e -> unlines [ "JSON Error:", show e ]
  E e -> unlines [ "Error:", p e ]

-- | Also logs the exception.
throwHttpException
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => HttpException
  -> HttpTT e r w s p t eff a
throwHttpException e = do
  logNow LogError $ errorMessage $ E_Http e
  throw $ E_Http e

-- | Re-throws other error types.
catchHttpException
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => HttpTT e r w s p t eff a
  -> (HttpException -> HttpTT e r w s p t eff a) -- ^ Handler
  -> HttpTT e r w s p t eff a
catchHttpException x handler = catch x $ \err ->
  case err of
    E_Http e -> handler e
    _ -> throw err

-- | Also logs the exception.
throwIOException
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => IOException
  -> HttpTT e r w s p t eff a
throwIOException e = do
  logNow LogError $ errorMessage $ E_IO e
  throw $ E_IO e

-- | Re-throws other error types.
catchIOException
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => HttpTT e r w s p t eff a
  -> (IOException -> HttpTT e r w s p t eff a) -- ^ Handler
  -> HttpTT e r w s p t eff a
catchIOException x handler = catch x $ \err ->
  case err of
    E_IO e -> handler e
    _ -> throw err

-- | Also logs the exception.
throwJsonError
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => JsonError
  -> HttpTT e r w s p t eff a
throwJsonError e = do
  logNow LogError $ errorMessage $ E_Json e
  throw $ E_Json e

-- | Re-throws other error types.
catchJsonError
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => HttpTT e r w s p t eff a
  -> (JsonError -> HttpTT e r w s p t eff a) -- ^ Handler
  -> HttpTT e r w s p t eff a
catchJsonError x handler = catch x $ \err ->
  case err of
    E_Json e -> handler e
    _ -> throw err

-- | Also logs the exception.
throwError
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => e
  -> HttpTT e r w s p t eff a
throwError e = do
  logNow LogError $ errorMessage $ E e
  throw $ E e

-- | Re-throws other error types.
catchError
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => HttpTT e r w s p t eff a
  -> (e -> HttpTT e r w s p t eff a) -- ^ Handler
  -> HttpTT e r w s p t eff a
catchError x handler = catch x $ \err ->
  case err of
    E e -> handler e
    _ -> throw err

-- | Handle any thrown error. To handle only errors of a specific type, see @catchError@, @catchJsonError@, @catchIOException@, or @catchHttpException@.
catchAnyError
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => HttpTT e r w s p t eff a
  -> (e -> HttpTT e r w s p t eff a)
  -> (HttpException -> HttpTT e r w s p t eff a)
  -> (IOException -> HttpTT e r w s p t eff a)
  -> (JsonError -> HttpTT e r w s p t eff a)
  -> HttpTT e r w s p t eff a
catchAnyError x hE hHttp hIO hJson =
  catch x $ \err -> case err of
    E e -> hE e
    E_Http e -> hHttp e
    E_IO e -> hIO e
    E_Json e -> hJson e



-- | Generic session environment.
data R e w r = R
  { _logOptions :: LogOptions e w

  -- | Printer for log entries.
  , _logEntryPrinter :: LogOptions e w -> LogEntry e w -> Maybe String

  -- | Handle for printing logs
  , _logHandle :: Handle

  -- | Lock used to prevent race conditions when writing to the log.
  , _logLock :: Maybe (MVar ())

  -- | Identifier string for the session; used to help match log entries emitted by the same session.
  , _uid :: String

  -- | Function for elevating 'HttpException's to a client-supplied error type.
  , _httpErrorInject :: HttpException -> Maybe e 

  -- | Client-supplied environment type.
  , _env :: r
  }

-- | Environment constructor
basicEnv
  :: (Show e, Show w)
  => r -- ^ Client-supplied environment value.
  -> R e w r
basicEnv r = R
  { _httpErrorInject = const Nothing
  , _logOptions = basicLogOptions
  , _logEntryPrinter = basicLogEntryPrinter
  , _logHandle = stdout
  , _logLock = Nothing
  , _uid = ""
  , _env = r
  }

-- | Environment constructor
trivialEnv
  :: r -- ^ Client-supplied environment value.
  -> R e w r
trivialEnv r = R
  { _httpErrorInject = const Nothing
  , _logOptions = trivialLogOptions
  , _logEntryPrinter = basicLogEntryPrinter
  , _logHandle = stdout
  , _logLock = Nothing
  , _uid = ""
  , _env = r
  }

-- | Options for tweaking the logs.
data LogOptions e w = LogOptions
  { -- | Toggle color
    _logColor :: Bool

    -- | Toggle JSON pretty printing
  , _logJson :: Bool

    -- | Toggle to silence the logs
  , _logSilent :: Bool

    -- | Suppress log output below this severity
  , _logMinSeverity :: LogSeverity

    -- | Toggle for printing HTTP headers
  , _logHeaders :: Bool

    -- | Printer for client-supplied error type. The boolean toggles JSON pretty printing.
  , _printUserError :: Bool -> e -> String

    -- | Printer for client-supplied log type. the boolean toggles JSON pretty printing.
  , _printUserLog :: Bool -> w -> String
  }

-- | Noisy, in color, without parsing JSON responses, and using `Show` instances for user-supplied error and log types.
basicLogOptions :: (Show e, Show w) => LogOptions e w
basicLogOptions = LogOptions
  { _logColor = True
  , _logJson = False
  , _logSilent = False
  , _logMinSeverity = LogDebug
  , _logHeaders = True
  , _printUserError = \_ e -> show e
  , _printUserLog = \_ w -> show w
  }

-- | Noisy, in color, without parsing JSON responses, and using trivial printers for user-supplied error and log types. For testing.
trivialLogOptions :: LogOptions e w
trivialLogOptions = LogOptions
  { _logColor = True
  , _logJson = False
  , _logSilent = False
  , _logMinSeverity = LogDebug
  , _logHeaders = True
  , _printUserError = \_ _ -> "ERROR"
  , _printUserLog = \_ _ -> "LOG"
  }

-- | Simple default pretty printer for @LogEntry@s.
basicLogEntryPrinter
  :: LogOptions e w
  -> LogEntry e w
  -> Maybe String
basicLogEntryPrinter opt@LogOptions{..} LogEntry{..} =
  if _logSilent || (_logEntrySeverity < _logMinSeverity)
    then Nothing
    else
      let
        colorize msg = if _logColor
          then colorBySeverity _logEntrySeverity msg
          else msg

        timestamp :: String
        timestamp = take 19 $ show _logEntryTimestamp
      in
        Just $ unwords $ filter (/= "")
          [ colorize timestamp
          , _logEntryUID
          , logEntryTitle _logEntry
          , logEntryBody opt _logEntry
          ]



-- | Log type
newtype W e w = W
  { unW :: [LogEntry e w]
  } deriving Show

instance Semigroup (W e w) where
  (W a1) <> (W a2) = W (a1 ++ a2)

instance Monoid (W e w) where
  mempty = W []
  mappend = (<>)

data LogEntry e w = LogEntry
  { _logEntryTimestamp :: UTCTime
  , _logEntryUID :: String
  , _logEntrySeverity :: LogSeverity
  , _logEntry :: Log e w
  } deriving Show

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

logEntryTitle :: Log e w -> LogEntryTitle
logEntryTitle e = case e of
  L_Comment _ -> "Comment"
  L_Request _ _ _ _ -> "Request"
  L_SilentRequest -> "Silent Request"
  L_Response _ -> "Response"
  L_SilentResponse -> "Silent Response"
  L_Pause _ -> "Pause"
  L_HttpError _ -> "HTTP Exception"
  L_IOError _ -> "IO Exception"
  L_JsonError _ -> "JSON Error"
  L_Error _ -> "Error"
  L_Log _ -> "Log"

-- | Used in the logs.
data HttpVerb
  = DELETE | GET | POST
  deriving (Eq, Show)

-- | All log statements should go through @logNow@.
printHttpLogs
  :: Handle
  -> Maybe (MVar ())
  -> LogOptions e w
  -> (LogOptions e w -> LogEntry e w -> Maybe String)
  -> W e w
  -> IO ()
printHttpLogs handle lock opts printer (W ws) = do
  let
    printEntry w = 
      case printer opts w of
        Nothing -> return ()
        Just str -> do
          case lock of
            Just lock -> withMVar lock (\() -> System.IO.hPutStrLn handle str)
            Nothing -> System.IO.hPutStrLn handle str
          hFlush handle

  if _logSilent opts
    then return ()
    else mapM_ printEntry ws



-- | Convert errors to log entries
errorMessage :: E e -> Log e w
errorMessage e = case e of
  E_Http err -> L_HttpError err
  E_IO err -> L_IOError err
  E_Json err -> L_JsonError err
  E e -> L_Error e

type LogEntryTitle = String
type LogEntryBody = String

logEntryBody
  :: LogOptions e w
  -> Log e w
  -> LogEntryBody
logEntryBody LogOptions{..} entry = case entry of
  L_Comment msg -> msg

  L_Request verb url opt payload ->
    let
      head = case (_logJson, _logHeaders) of
        (True,  True)  -> unpack $ encodePretty $ jsonResponseHeaders $ opt ^. Wreq.headers
        (False, True)  -> show $ opt ^. Wreq.headers
        (_,     False) -> ""

      body = case (_logJson, payload) of
        (True,  Just p)  -> case decode p of
          Nothing -> "JSON parse error:\n" ++ unpack p
          Just v -> unpack $ encodePretty (v :: Value)
        (False, Just p)  -> unpack p
        (_,     Nothing) -> ""

    in
      intercalate "\n" $ filter (/= "") [unwords ["Request", show verb, url], head, body]

  L_SilentRequest -> ""

  L_Response response ->
    let
      head = case (_logJson, _logHeaders) of
        (True,  True)  -> unpack $ encodePretty $ jsonResponseHeaders $ _responseHeaders response
        (False, True)  -> show $ _responseHeaders response
        (_,     False) -> ""

      body = case _logJson of
        True  -> unpack $ encodePretty $ preview _Value $ _responseBody response
        False -> show response

    in
      intercalate "\n" $ filter (/= "") ["Response", head, body]

  L_SilentResponse -> ""

  L_Pause k -> "Wait for " ++ show k ++ "μs"

  L_HttpError e -> if _logJson
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
          Nothing -> show e
          Just (code, json) -> intercalate "\n" [ unwords [ "HTTP Error Response", code], json ]

    else show e

  L_IOError e -> unwords [ show $ ioeGetFileName e, ioeGetLocation e, ioeGetErrorString e ]

  L_JsonError e -> show e

  L_Error e -> unwords [ _printUserError _logJson e ]

  L_Log w -> unwords [ _printUserLog _logJson w ]



-- | Extract the user-defined log entries.
logEntries :: W e w -> [w]
logEntries (W xs) = entries xs
  where
    entries [] = []
    entries (w:ws) = case _logEntry w of
      L_Log u -> u : entries ws
      _ -> entries ws



-- | State type
data S s = S
  { _httpOptions :: Wreq.Options
  , _httpSession :: Maybe S.Session
  , _userState :: s
  } deriving Show

-- | State constructor
basicState :: s -> S s
basicState s = S
  { _httpOptions = Wreq.defaults
  , _httpSession = Nothing
  , _userState = s
  }



-- | Atomic effects
data P p a where
  HPutStrLn
    :: Handle -> String
    -> P p (Either IOException ())
  HPutStrLnBlocking
    :: MVar () -> Handle -> String
    -> P p (Either IOException ())

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
  HPutStrLn handle string -> try $ do
    System.IO.hPutStrLn handle string
    hFlush handle

  HPutStrLnBlocking lock handle str -> try $ do
    withMVar lock (\() -> System.IO.hPutStrLn handle str)
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

-- | Basic evaluator for interpreting atomic 'Http' effects in 'MockIO'.
evalMockIO
  :: (p a -> MockIO s a)
  -> P p a
  -> MockIO s a
evalMockIO eval x = case x of
  HPutStrLn handle str -> do
    incrementTimer 1
    fmap Right $ modifyMockWorld $ \w -> w
      { _files = appendLines (Right handle) (lines str) $ _files w }

  HPutStrLnBlocking _ handle str -> do
    incrementTimer 1
    fmap Right $ modifyMockWorld $ \w -> w
      { _files = appendLines (Right handle) (lines str) $ _files w }

  GetSystemTime -> do
    incrementTimer 1
    MockWorld{..} <- getMockWorld
    return _time

  ThreadDelay k -> incrementTimer k

  HttpGet _ _ url -> do
    incrementTimer 1
    MockWorld{..} <- getMockWorld
    let (r,t) = unMockNetwork (_httpGet url) _serverState
    modifyMockWorld $ \w -> w { _serverState = t }
    return r

  HttpPost _ _ url payload -> do
    incrementTimer 1
    MockWorld{..} <- getMockWorld
    let (r,t) = unMockNetwork (_httpPost url payload) _serverState
    modifyMockWorld $ \w -> w { _serverState = t }
    return r

  HttpDelete _ _ url -> do
    incrementTimer 1
    MockWorld{..} <- getMockWorld
    let (r,t) = unMockNetwork (_httpDelete url) _serverState
    modifyMockWorld $ \w -> w { _serverState = t }
    return r

  P p -> do
    incrementTimer 1
    eval p



-- | All log statements should go through @logNow@.
logNow
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => LogSeverity
  -> Log e w
  -> HttpTT e r w s p t eff ()
logNow severity msg = do
  time <- prompt GetSystemTime
  printer <- reader _logEntryPrinter
  R{..} <- ask
  case printer _logOptions (LogEntry time _uid severity msg) of
    Nothing -> return ()
    Just str -> case _logLock of
      Just lock -> hPutStrLnBlocking lock _logHandle str
      Nothing -> Control.Monad.Script.Http.hPutStrLn _logHandle str
  tell $ W [LogEntry time _uid severity msg]

-- | Write a comment to the log
comment
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => String
  -> HttpTT e r w s p t eff ()
comment msg = logNow LogInfo $ L_Comment msg

-- | Pause the thread
wait
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => Int -- ^ milliseconds
  -> HttpTT e r w s p t eff ()
wait k = do
  logNow LogInfo $ L_Pause k
  prompt $ ThreadDelay k

-- | Write an entry to the log
logEntry
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => LogSeverity -> w -> HttpTT e r w s p t eff ()
logEntry severity = logNow severity . L_Log

-- | For debug level messages
logDebug
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => w -> HttpTT e r w s p t eff ()
logDebug = logEntry LogDebug

-- | For informational messages
logInfo
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => w -> HttpTT e r w s p t eff ()
logInfo = logEntry LogInfo

-- | For normal but significant conditions
logNotice
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => w -> HttpTT e r w s p t eff ()
logNotice = logEntry LogNotice

-- | For warning conditions
logWarning
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => w -> HttpTT e r w s p t eff ()
logWarning = logEntry LogWarning

-- | For error conditions
logError
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => w -> HttpTT e r w s p t eff ()
logError = logEntry LogError

-- | For critical conditions
logCritical
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => w -> HttpTT e r w s p t eff ()
logCritical = logEntry LogCritical

-- | Action must be taken immediately
logAlert
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => w -> HttpTT e r w s p t eff ()
logAlert = logEntry LogAlert

-- | System is unusable
logEmergency
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => w -> HttpTT e r w s p t eff ()
logEmergency = logEntry LogEmergency

-- | Set the severity level of all log actions in a session.
setLogSeverity
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => LogSeverity
  -> HttpTT e r w s p t eff a
  -> HttpTT e r w s p t eff a
setLogSeverity severity = censor (W . map f . unW)
  where
    f :: LogEntry e w -> LogEntry e w
    f e = e { _logEntrySeverity = severity }



-- | Write a line to a handle
hPutStrLn
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => Handle
  -> String
  -> HttpTT e r w s p t eff ()
hPutStrLn h str = do
  result <- prompt $ HPutStrLn h str
  case result of
    Right () -> return ()
    Left e -> throwIOException e

-- | Write a line to a handle, using the given `MVar` as a lock
hPutStrLnBlocking
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => MVar ()
  -> Handle
  -> String
  -> HttpTT e r w s p t eff ()
hPutStrLnBlocking lock h str = do
  result <- prompt $ HPutStrLnBlocking lock h str
  case result of
    Right () -> return ()
    Left e -> throwIOException e



-- | Run a @GET@ request
httpGet
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => Url
  -> HttpTT e r w s p t eff HttpResponse
httpGet url = do
  R{..} <- ask
  S{..} <- get
  logNow LogDebug $ L_Request GET url _httpOptions Nothing
  result <- prompt $ HttpGet _httpOptions _httpSession url
  case result of
    Right response -> do
      logNow LogDebug $ L_Response response
      return response
    Left err -> case _httpErrorInject err of
      Just z -> throwError z
      Nothing -> throwHttpException err

-- | Run a @GET@ request, but do not write the request or response to the logs.
httpSilentGet
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => Url
  -> HttpTT e r w s p t eff HttpResponse
httpSilentGet url = do
  R{..} <- ask
  S{..} <- get
  logNow LogDebug L_SilentRequest
  result <- prompt $ HttpGet _httpOptions _httpSession url
  case result of
    Right response -> do
      logNow LogDebug L_SilentResponse
      return response
    Left err -> case _httpErrorInject err of
      Just z -> throwError z
      Nothing -> throwHttpException err

-- | Run a @POST@ request
httpPost
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => Url
  -> ByteString -- ^ Payload
  -> HttpTT e r w s p t eff HttpResponse
httpPost url payload = do
  R{..} <- ask
  S{..} <- get
  logNow LogDebug $ L_Request POST url _httpOptions (Just payload)
  result <- prompt $ HttpPost _httpOptions _httpSession url payload
  case result of
    Right response -> do
      logNow LogDebug $ L_Response response
      return response
    Left err -> case _httpErrorInject err of
      Just z -> throwError z
      Nothing -> throwHttpException err

-- | Run a @POST@ request, but do not write the request or response to the logs.
httpSilentPost
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => Url
  -> ByteString -- ^ Payload
  -> HttpTT e r w s p t eff HttpResponse
httpSilentPost url payload = do
  R{..} <- ask
  S{..} <- get
  logNow LogDebug L_SilentRequest
  result <- prompt $ HttpPost _httpOptions _httpSession url payload
  case result of
    Right response -> do
      logNow LogDebug L_SilentResponse
      return response
    Left err -> case _httpErrorInject err of
      Just z -> throwError z
      Nothing -> throwHttpException err

-- | Run a @DELETE@ request
httpDelete
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => Url
  -> HttpTT e r w s p t eff HttpResponse
httpDelete url = do
  R{..} <- ask
  S{..} <- get
  logNow LogDebug $ L_Request DELETE url _httpOptions Nothing
  result <- prompt $ HttpDelete _httpOptions _httpSession url
  case result of
    Right response -> do
      logNow LogDebug$ L_Response response
      return response
    Left err -> case _httpErrorInject err of
      Just z -> throwError z
      Nothing -> throwHttpException err

-- | Run a @DELETE@ request, but do not write the request or response to the logs.
httpSilentDelete
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => Url
  -> HttpTT e r w s p t eff HttpResponse
httpSilentDelete url = do
  R{..} <- ask
  S{..} <- get
  logNow LogDebug L_SilentRequest
  result <- prompt $ HttpDelete _httpOptions _httpSession url
  case result of
    Right response -> do
      logNow LogDebug L_SilentResponse
      return response
    Left err -> case _httpErrorInject err of
      Just z -> throwError z
      Nothing -> throwHttpException err



-- | Parse a `ByteString` to a JSON `Value`.
parseJson
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => ByteString
  -> HttpTT e r w s p t eff Value
parseJson bytes = case preview _Value bytes of
  Just value -> return value
  Nothing -> throwJsonError $ JsonParseError bytes

-- | Object member lookup.
lookupKeyJson
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => Text -- ^ Key name
  -> Value -- ^ JSON object
  -> HttpTT e r w s p t eff Value
lookupKeyJson key v = case v of
  Object obj -> case lookup key obj of
    Nothing -> throwJsonError $ JsonKeyDoesNotExist key (Object obj)
    Just value -> return value
  _ -> throwJsonError $ JsonKeyLookupOffObject key v

-- | Decode a `A.Value` to some other type.
constructFromJson
  :: (Monad eff, Monad (t eff), MonadTrans t, FromJSON a)
  => Value
  -> HttpTT e r w s p t eff a
constructFromJson value = case fromJSON value of
  Success x -> return x
  Error msg -> throwJsonError $ JsonConstructError msg
