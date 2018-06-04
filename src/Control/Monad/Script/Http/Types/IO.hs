{-# LANGUAGE GADTs #-}
module Control.Monad.Script.Http.Types.IO (
    evalIO
) where

import Control.Exception (Exception, try)
import Data.Time (UTCTime)
import Data.Time.Clock.System (getSystemTime, systemToUTCTime)
import Data.ByteString.Lazy (ByteString, readFile, writeFile)
import Control.Concurrent (threadDelay)
import System.IO
  ( Handle, hPutStrLn, hGetEcho, hSetEcho, hFlush
  , hFlush, hGetLine, hPutStr, hPutChar )
import Control.Concurrent.MVar (MVar, withMVar)
import Network.Wreq (Options, getWith, postWith, deleteWith)
import qualified Network.Wreq.Session as S
  ( Session, newSession, getWith, postWith, deleteWith )

import Control.Monad.Script.Http.Types (P(..), readHttpResponse)

evalIO :: (p a -> IO a) -> P p a -> IO a
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
