{- |
Module      : Control.Monad.Script.Http.Types.IO
Description : An IO evaluator for HTTP interactions.
Copyright   : 2018, Automattic, Inc.
License     : BSD3
Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
Stability   : experimental
Portability : POSIX

A basic IO evaluator for atomic effects in the 'Http' monad.
-}

{-# LANGUAGE GADTs #-}
module Control.Monad.Script.Http.Types.IO (
    evalIO
) where


import Control.Concurrent
  ( threadDelay )
import Control.Concurrent.MVar
  ( MVar, withMVar )
import Control.Exception
  ( Exception, try )
import Data.ByteString.Lazy
  ( ByteString, readFile, writeFile )
import Data.Time
  ( UTCTime )
import Data.Time.Clock.System
  ( getSystemTime, systemToUTCTime )
import Network.HTTP.Client
  ( Response, responseCookieJar, responseBody
  , responseHeaders, responseVersion, responseStatus )
import Network.Wreq
  ( Options, getWith, postWith, deleteWith )
import qualified Network.Wreq.Session as S
  ( Session, newSession, getWith, postWith, deleteWith )
import System.IO
  ( Handle, hPutStrLn, hGetEcho, hSetEcho, hFlush
  , hFlush, hGetLine, hPutStr, hPutChar )


import Control.Monad.Script.Http.Types (P(..), HttpResponse(..))



-- ^ Basic evaluator for interpreting atomic 'Http' effects in 'IO'.
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

  HGetLine handle -> do
    hFlush handle
    hGetLine handle

  HGetLineNoEcho handle -> do
    hFlush handle
    echo <- hGetEcho handle
    hSetEcho handle False
    secret <- hGetLine handle
    hSetEcho handle echo
    return secret

  GetSystemTime -> fmap systemToUTCTime getSystemTime

  ThreadDelay k -> threadDelay k

  HttpGet opts s url -> case s of
    Nothing -> try $ readHttpResponse <$> getWith opts url
    Just sn -> try $ readHttpResponse <$> S.getWith opts sn url

  HttpPost opts s url msg -> case s of
    Nothing -> try $ readHttpResponse <$> postWith opts url msg
    Just sn -> try $ readHttpResponse <$> S.postWith opts sn url msg

  HttpDelete opts s url -> case s of
    Nothing -> try $ readHttpResponse <$> deleteWith opts url
    Just sn -> try $ readHttpResponse <$> S.deleteWith opts sn url

  P act -> eval act



-- | Convert a `Response ByteString` into an `HttpResponse`.
readHttpResponse :: Response ByteString -> HttpResponse
readHttpResponse r = HttpResponse
  { _responseStatus = responseStatus r
  , _responseVersion = responseVersion r
  , _responseHeaders = responseHeaders r
  , _responseBody = responseBody r
  , _responseCookieJar = responseCookieJar r
  }
