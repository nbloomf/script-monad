{- |
Module      : Control.Monad.Script.Http.Types
Description : Types and helper functions.
Copyright   : 2018, Automattic, Inc.
License     : BSD3
Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
Stability   : experimental
Portability : POSIX

Types and helper functions for effects in the 'Http' monad.
-}

{-# LANGUAGE GADTs, RecordWildCards #-}
module Control.Monad.Script.Http.Types (
  -- * Errors
    E(..)
  , JsonError(..)

  -- * Reader
  , R(..)
  , LogOptions(..)
  , basicEnv

  -- * Writer
  , W(..)
  , Log(..)
  , userLog
  , Url
  , HttpVerb(..)
  , HttpResponse(..)
  , Color(..)
  , printLogWith
  , errorMessage

  -- * State
  , S(..)
  , basicState

  -- * Prompt
  , P(..)
) where



import Control.Concurrent.MVar
  ( MVar )
import Control.Exception
  ( IOException )
import Control.Lens
  ( preview, (^.) )
import Data.Aeson
  ( Value, decode )
import Data.Aeson.Encode.Pretty
  ( encodePretty )
import Data.Aeson.Lens
  ( _Value )
import Data.ByteString.Lazy
  ( ByteString, fromStrict )
import Data.ByteString.Lazy.Char8
  ( unpack, pack )
import Data.Time
  ( UTCTime )
import Network.HTTP.Client
  ( HttpException(..), CookieJar, HttpExceptionContent(StatusCodeException) )
import Network.HTTP.Types
  ( HttpVersion, Status, ResponseHeaders )
import qualified Network.Wreq as Wreq
  ( Options, defaults, responseStatus )
import Network.Wreq.Session
  ( Session )
import System.IO
  ( Handle, stdout, stdin )
import System.IO.Error
  ( ioeGetFileName, ioeGetLocation, ioeGetErrorString )
import qualified Data.Text as T (Text)



-- | Error type
data E e
  = E_Http HttpException
  | E_IO IOException
  | E_Json JsonError
  | E e -- ^ Client-supplied error type.



-- | Represents the kinds of errors that can occur when parsing and decoding JSON.
data JsonError
  = JsonError -- ^ A generic JSON error; try not to use this.
  | JsonParseError -- ^ A failed parse.
  | JsonKeyDoesNotExist T.Text -- ^ An attempt to look up the value of a key that does not exist on an object.
  | JsonKeyLookupOffObject T.Text -- ^ An attempt to look up the value of a key on something other than an object.
  | JsonConstructError String -- ^ A failed attempt to convert a `Value` to some other type.
  deriving (Eq, Show)



-- | Generic session environment.
data R e w r = R
  { _logOptions :: LogOptions e w
  , _logHandle :: Handle
  , _logLock :: MVar () -- ^ Lock used to prevent race conditions when writing to the log.
  , _stdout :: Handle -- ^ Handle to treat as @stdout@.
  , _stdin :: Handle -- ^ Handle to treat as @stdin@.
  , _uid :: String
  , _httpErrorInject :: HttpException -> Maybe e -- ^ Function for elevating 'HttpException's to a client-supplied error type.
  , _userEnv :: r -- ^ Client-supplied environment type.
  }



-- | Options for tweaking the logs.
data LogOptions e w = LogOptions
  { _logColor :: Bool -- ^ Toggle color
  , _logJson :: Bool -- ^ Toggle JSON pretty printing
  , _logSilent :: Bool -- ^ Toggle to silence the logs
  , _printUserError :: Bool -> e -> String -- ^ Printer for client-supplied error type; the boolean toggles JSON pretty printing.
  , _printUserLog :: Bool -> w -> String -- ^ Printer for client-supplied log type; the boolean toggles JSON pretty printing.
  }



-- | Environment constructor
basicEnv
  :: MVar () -- ^ Lock; used to prevent race conditions when writing to the log.
  -> (Bool -> e -> String) -- ^ Printer for client-supplied error type; the boolean argument toggles JSON pretty printing.
  -> (Bool -> w -> String) -- ^ Printer for client-supplied log type; the boolean argument toggles JSON pretty printing.
  -> r -- ^ Client-supplied environment value.
  -> R e w r
basicEnv lock printError printLog r = R
  { _httpErrorInject = const Nothing
  , _logOptions = LogOptions
      { _logColor = True
      , _logJson = False
      , _logSilent = False
      , _printUserError = printError
      , _printUserLog = printLog
      }
  , _logHandle = stdout
  , _logLock = lock
  , _stdout = stdout
  , _stdin = stdin
  , _uid = ""
  , _userEnv = r
  }



-- | Log type
data W e w = W [(UTCTime, Log e w)]

instance Monoid (W e w) where
  mempty = W []
  mappend (W a1) (W a2) = W (a1 ++ a2)

userLog :: W e w -> [w]
userLog (W ws) = foo ws
  where
    foo [] = []
    foo ((_,v):vs) = case v of
      L_Log u -> u : foo vs
      _ -> foo vs



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
  | L_Error e -- ^ Client-supplied error type
  | L_Log w -- ^ Client-supplied log entry type

type Url = String

-- | Used in the logs
data HttpVerb
  = DELETE | GET | POST
  deriving Show

data HttpResponse = HttpResponse
  { _responseStatus :: Status
  , _responseVersion :: HttpVersion
  , _responseHeaders :: ResponseHeaders
  , _responseBody :: ByteString
  , _responseCookieJar :: CookieJar
  } deriving Show

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



-- | State type
data S s = S
  { _httpOptions :: Wreq.Options
  , _httpSession :: Maybe Session
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
  HGetLine :: Handle -> P p String
  HGetLineNoEcho :: Handle -> P p String

  GetSystemTime :: P p UTCTime
  ThreadDelay :: Int -> P p ()

  HttpGet
    :: Wreq.Options -> Maybe Session -> Url
    -> P p (Either HttpException HttpResponse)
  HttpPost
    :: Wreq.Options -> Maybe Session -> Url
    -> ByteString -> P p (Either HttpException HttpResponse)
  HttpDelete
    :: Wreq.Options -> Maybe Session -> Url
    -> P p (Either HttpException HttpResponse)

  P :: p a -> P p a
