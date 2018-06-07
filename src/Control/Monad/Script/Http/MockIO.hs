{- |
Module      : Control.Monad.Script.Http.MockIO
Description : A mock IO monad for testing.
Copyright   : 2018, Automattic, Inc.
License     : BSD3
Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
Stability   : experimental
Portability : POSIX

A fake IO monad for testing `Http`.
-}

{-# LANGUAGE GADTs, RecordWildCards, ScopedTypeVariables, Rank2Types #-}
module Control.Monad.Script.Http.MockIO (
  -- * MockIO
    MockIO(..)
  , execHttpMockIO
  , evalMockIO
  , MockWorld(..)
  , getMockWorld
  , putMockWorld
  , modifyMockWorld
  , modifyMockServerState
  , MockNetwork(..)
  , MockServer(..)

  -- * Helpers
  , mockGetFile
  , mockHPutStrLn
  , mockHGetLine
  , epoch

  -- * Responses
  , _200ok
  , _400badRequest
  , _404notFound
  , _405methodNotAllowed
  , _408requestTimeout
  , _500internalServerError

  -- * Testing
  , checkHttpM
  , hasValueWith
  , hasErrorWith
  , hasLogEntriesWith
  , checkHttpMockIO
  , theResult
  , hasWorldWith
) where



import Control.Exception
  ( Exception, SomeException, fromException )
import Control.Monad
  ( ap )
import Data.ByteString.Lazy
  ( ByteString, pack )
import Data.Time
  ( UTCTime(..), Day(..) )
import Data.Time.Clock
  ( addUTCTime )
import Network.HTTP.Client
  ( HttpException, createCookieJar )
import Network.HTTP.Types
import System.IO

import Control.Monad.Script.Http
  ( P(..), E, R, W, S, Http, HttpResponse(..), execHttpM, logEntries )





-- | A state monad over `MockWorld`.
data MockIO s a = MockIO
  { runMockIO :: MockWorld s -> (a, MockWorld s) }

instance Monad (MockIO s) where
  return x = MockIO $ \s -> (x,s)

  (MockIO x) >>= f = MockIO $ \s ->
    let (z,t) = x s in runMockIO (f z) t
        { _time = addUTCTime 1 $ _time t }

instance Applicative (MockIO s) where
  pure = return
  (<*>) = ap

instance Functor (MockIO s) where
  fmap f x = x >>= (return . f)

data MockWorld s = MockWorld
  { _files :: [(Handle, [String])]
  , _time :: UTCTime

  , _httpGet :: String -> MockNetwork s HttpResponse
  , _httpPost :: String -> ByteString -> MockNetwork s HttpResponse
  , _httpDelete :: String -> MockNetwork s HttpResponse

  , _serverState :: MockServer s
  }

getMockWorld :: MockIO s (MockWorld s)
getMockWorld = MockIO $ \s -> (s,s)

putMockWorld :: MockWorld s -> MockIO s ()
putMockWorld s = MockIO $ \_ -> ((),s)

modifyMockWorld :: (MockWorld s -> MockWorld s) -> MockIO s ()
modifyMockWorld f = MockIO $ \s -> ((), f s)

modifyMockServerState :: (s -> s) -> MockIO s ()
modifyMockServerState f = modifyMockWorld $ \w -> w
  { _serverState = f $ _serverState w
  }

execHttpMockIO
  :: S s
  -> R e w r
  -> (forall a. p a -> MockIO u a)
  -> MockWorld u
  -> Http e r w s p t
  -> ((Either (E e) t, S s, W e w), MockWorld u)
execHttpMockIO st env eval world http =
  runMockIO (execHttpM st env (evalMockIO eval) http) world





data MockNetwork s a = MockNetwork
  { unMockNetwork :: MockServer s -> (Either HttpException a, MockServer s) }

instance Monad (MockNetwork s) where
  return x = MockNetwork $ \s -> (Right x, s)

  (MockNetwork x) >>= f = MockNetwork $ \s ->
    let (z,t) = x s in
    case z of
      Left e -> (Left e, t)
      Right a -> unMockNetwork (f a) t

instance Applicative (MockNetwork s) where
  pure = return
  (<*>) = ap

instance Functor (MockNetwork s) where
  fmap f x = x >>= (return . f)

getMockServer :: MockNetwork s (MockServer s)
getMockServer = MockNetwork $ \s -> (Right s,s)

putMockServer :: MockServer s -> MockNetwork s ()
putMockServer s = MockNetwork $ \_ -> (Right (),s)

modifyMockServer :: (MockServer s -> MockServer s) -> MockNetwork s ()
modifyMockServer f = MockNetwork $ \s -> (Right (), f s)

data MockServer s = MockServer s





mockGetFile :: (Eq k) => k -> [(k,v)] -> Maybe v
mockGetFile k xs = case xs of
  [] -> Nothing
  (u,x):rest -> if k == u
    then Just x
    else mockGetFile k rest

mockHPutStrLn :: (Eq k, Monoid v) => (k,v) -> [(k,v)] -> [(k,v)]
mockHPutStrLn (k,v) xs = case xs of
  [] -> [(k,v)]
  (u,x):rest -> if k == u
    then (k, mappend x v):rest
    else (u,x) : mockHPutStrLn (k,v) rest

mockHGetLine
  :: (Eq k)
  => e -- ^ Handle not found
  -> e -- ^ EOF
  -> k
  -> [(k,[a])]
  -> Either e (a, [(k,[a])])
mockHGetLine notFound eof k fs = getline fs []
  where
    getline xs ys = case xs of
      [] -> Left notFound
      (u,x):rest -> if k == u
        then case x of
          [] -> Left eof
          w:ws -> Right (w, [(k,ws)] ++ rest ++ ys)
        else getline rest ((u,x):ys)

epoch :: UTCTime
epoch = UTCTime (ModifiedJulianDay 0) 0





evalMockIO :: (p a -> MockIO s a) -> P p a -> MockIO s a
evalMockIO eval x = case x of
  HPutStrLn handle str -> modifyMockWorld $
    \w -> w { _files = mockHPutStrLn (handle, lines str) $ _files w }

  HPutStrLnBlocking _ handle str -> modifyMockWorld $
    \w -> w { _files = mockHPutStrLn (handle, lines str) $ _files w }

  GetSystemTime -> do
    MockWorld{..} <- getMockWorld
    return _time

  ThreadDelay k -> return ()

  HttpGet _ _ url -> do
    MockWorld{..} <- getMockWorld
    let (r,t) = unMockNetwork (_httpGet url) _serverState
    modifyMockWorld $ \w -> w { _serverState = t }
    return r

  HttpPost _ _ url payload -> do
    MockWorld{..} <- getMockWorld
    let (r,t) = unMockNetwork (_httpPost url payload) _serverState
    modifyMockWorld $ \w -> w { _serverState = t }
    return r

  HttpDelete _ _ url -> do
    MockWorld{..} <- getMockWorld
    let (r,t) = unMockNetwork (_httpDelete url) _serverState
    modifyMockWorld $ \w -> w { _serverState = t }
    return r

  P p -> eval p





checkHttpM
  :: (Monad m)
  => S s
  -> R e w r
  -> (forall a. P p a -> m a)
  -> (forall a. m a -> IO a)
  -> ((Either (E e) t, S s, W e w) -> Bool)
  -> Http e r w s p t
  -> IO Bool
checkHttpM st env eval toIO check http =
  fmap check $ toIO $ execHttpM st env eval http

hasValueWith
  :: (t -> Bool)
  -> (Either (E e) t, S s, W e w)
  -> Bool
hasValueWith f (x,_,_) = case x of
  Right a -> f a
  Left _ -> False

hasErrorWith
  :: (E e -> Bool)
  -> (Either (E e) t, S s, W e w)
  -> Bool
hasErrorWith f (x,_,_) = case x of
  Left e -> f e
  Right _ -> False

hasLogEntriesWith
  :: ([w] -> Bool)
  -> (Either (E e) t, S s, W e w)
  -> Bool
hasLogEntriesWith f (_,_,w) = f $ logEntries w

checkHttpMockIO
  :: S s
  -> R e w r
  -> MockWorld u
  -> (forall a. P p a -> MockIO u a)
  -> (((Either (E e) t, S s, W e w), MockWorld u) -> Bool)
  -> Http e r w s p t
  -> Bool
checkHttpMockIO st env world eval check http =
  check $ runMockIO (execHttpM st env eval http) world

theResult
  :: ((Either (E e) t, S s, W e w) -> Bool)
  -> ((Either (E e) t, S s, W e w), MockWorld u)
  -> Bool
theResult f (x,_) = f x

hasWorldWith
  :: (MockWorld u -> Bool)
  -> ((Either (E e) t, S s, W e w), MockWorld u)
  -> Bool
hasWorldWith f (_,x) = f x





_200ok :: ByteString -> HttpResponse
_200ok body = HttpResponse
  { _responseStatus = status200
  , _responseVersion = http11
  , _responseHeaders = []
  , _responseBody = body
  , _responseCookieJar = createCookieJar []
  }

_400badRequest :: ByteString -> HttpResponse
_400badRequest body = HttpResponse
  { _responseStatus = status400
  , _responseVersion = http11
  , _responseHeaders = []
  , _responseBody = body
  , _responseCookieJar = createCookieJar []
  }

_404notFound :: ByteString -> HttpResponse
_404notFound body = HttpResponse
  { _responseStatus = status404
  , _responseVersion = http11
  , _responseHeaders = []
  , _responseBody = body
  , _responseCookieJar = createCookieJar []
  }

_405methodNotAllowed :: ByteString -> HttpResponse
_405methodNotAllowed body = HttpResponse
  { _responseStatus = status405
  , _responseVersion = http11
  , _responseHeaders = []
  , _responseBody = body
  , _responseCookieJar = createCookieJar []
  }

_408requestTimeout :: ByteString -> HttpResponse
_408requestTimeout body = HttpResponse
  { _responseStatus = status408
  , _responseVersion = http11
  , _responseHeaders = []
  , _responseBody = body
  , _responseCookieJar = createCookieJar []
  }

_500internalServerError :: ByteString -> HttpResponse
_500internalServerError body = HttpResponse
  { _responseStatus = status500
  , _responseVersion = http11
  , _responseHeaders = []
  , _responseBody = body
  , _responseCookieJar = createCookieJar []
  }
