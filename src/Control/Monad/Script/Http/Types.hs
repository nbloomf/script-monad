{-# LANGUAGE GADTs, RecordWildCards #-}
module Control.Monad.Script.Http.Types (
    E(..)
  , R(..)
  , W(..)
  , Log(..)
  , LogOptions(..)
  , printLogWith
  , S(..)
  , P(..)

  , HttpVerb(..)

  , errorMessage

  , basicState
  , basicEnv
  , Url

  , HttpResponse
  , readHttpResponse
) where


import Data.Aeson
  ( Value, decode )
import Data.Aeson.Lens
  ( _Value )
import Control.Exception (IOException)
import Data.Time (UTCTime)
import Data.ByteString.Lazy (ByteString, fromStrict)
import System.IO (Handle, stdout, stdin)
import System.IO.Error
  ( ioeGetFileName, ioeGetLocation, ioeGetErrorString )
import Control.Concurrent.MVar (MVar)
import Network.HTTP.Client
  ( HttpException(..), CookieJar, Response, responseCookieJar
  , responseBody, responseHeaders, responseVersion, responseStatus
  , HttpExceptionContent(StatusCodeException) )
import Network.HTTP.Types (HttpVersion, Status, ResponseHeaders)
import qualified Network.Wreq as Wreq (Options, defaults, responseStatus)
import Network.Wreq.Session (Session)
import Control.Lens
import Data.Aeson.Encode.Pretty
  ( encodePretty )
import Data.ByteString.Lazy.Char8
  ( unpack, pack )



data E e
  = E_Http HttpException
  | E_IO IOException
  | E_Json
  | E e



data R e w r = R
  { _httpErrorInject :: HttpException -> Maybe e
  , _logOptions :: LogOptions e w
  , _logHandle :: Handle
  , _logLock :: MVar ()
  , _stdout :: Handle
  , _stdin :: Handle
  , _uid :: String
  , _userEnv :: r
  }

data LogOptions e w = LogOptions
  { _logColor :: Bool
  , _logJson :: Bool
  , _logSilent :: Bool
  , _printUserError :: Bool -> e -> String
  , _printUserLog :: Bool -> w -> String
  }

basicEnv :: MVar () -> (Bool -> e -> String) -> (Bool -> w -> String) -> r -> R e w r
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



data W e w = W [(UTCTime, Log e w)]

instance Monoid (W e w) where
  mempty = W []
  mappend (W a1) (W a2) = W (a1 ++ a2)

data Log e w
  = L_Comment String
  | L_Request HttpVerb Url Wreq.Options (Maybe ByteString)
  | L_SilentRequest
  | L_Response HttpResponse
  | L_SilentResponse
  | L_Pause Int
  | L_HttpError HttpException
  | L_IOError IOException
  | L_JsonError
  | L_Error e
  | L_Log w

type Url = String

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

-- | Convert a `Response ByteString` into an `HttpResponse`.
readHttpResponse :: Response ByteString -> HttpResponse
readHttpResponse r = HttpResponse
  { _responseStatus = responseStatus r
  , _responseVersion = responseVersion r
  , _responseHeaders = responseHeaders r
  , _responseBody = responseBody r
  , _responseCookieJar = responseCookieJar r
  }

errorMessage :: E e -> Log e w
errorMessage e = case e of
  E_Http err -> L_HttpError err
  E_IO err -> L_IOError err
  E_Json -> L_JsonError
  E e -> L_Error e

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

  L_JsonError -> (Red, "JSON Error")

  L_Error e -> (Red, unwords [ "ERROR", printError asJson e ])

  L_Log w -> (Yellow, unwords [ "INFO", printLog asJson w ])



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




data S s = S
  { _httpOptions :: Wreq.Options
  , _httpSession :: Maybe Session
  , _lastResponse :: Maybe HttpResponse
  , _userState :: s
  }

basicState :: s -> S s
basicState s = S
  { _httpOptions = Wreq.defaults
  , _httpSession = Nothing
  , _lastResponse = Nothing
  , _userState = s
  }



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
