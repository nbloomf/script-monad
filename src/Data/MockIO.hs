{- |
Module      : Data.MockIO
Description : A mock IO monad for testing.
Copyright   : 2018, Automattic, Inc.
License     : BSD3
Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
Stability   : experimental
Portability : POSIX

A fake IO monad for testing.
-}

{-# LANGUAGE OverloadedStrings, GADTs, RecordWildCards, ScopedTypeVariables, Rank2Types #-}
module Data.MockIO (
  -- * MockIO
    MockIO(..)
  , getMockWorld
  , putMockWorld
  , modifyMockWorld
  , incrementTimer

  -- * MockWorld
  , MockWorld(..)
  , MockServer(..)
  , epoch
  , basicMockWorld

  -- * MockNetwork
  , MockNetwork(..)
  , errorMockNetwork
  , getMockServer
  , putMockServer
  , modifyMockServer

  -- * Responses
  , _200ok
  , _400badRequest
  , _404notFound
  , _405methodNotAllowed
  , _408requestTimeout
  , _500internalServerError
) where



import Control.Exception
  ( Exception, SomeException, fromException )
import Control.Monad
  ( ap )
import Data.ByteString.Lazy
  ( ByteString, pack )
import Data.Time
  ( UTCTime(..), Day(..), diffTimeToPicoseconds )
import Data.Time.Clock
  ( addUTCTime )
import Network.HTTP.Client
  ( HttpException, createCookieJar )
import System.IO
  ( Handle )
import Test.QuickCheck
  ( Arbitrary(..), CoArbitrary(..), variant )

import Data.MockIO.FileSystem
import Network.HTTP.Client.Extras



-- | A state monad over `MockWorld`.
data MockIO s a = MockIO
  { runMockIO :: MockWorld s -> (a, MockWorld s) }

instance Monad (MockIO s) where
  return x = MockIO $ \s -> (x,s)

  (MockIO x) >>= f = MockIO $ \s ->
    let (z,t) = x s in runMockIO (f z) t

instance Applicative (MockIO s) where
  pure = return
  (<*>) = ap

instance Functor (MockIO s) where
  fmap f x = x >>= (return . f)

instance Show (MockIO s a) where
  show _ = "<MockIO>"

instance (Arbitrary a) => Arbitrary (MockIO s a) where
  arbitrary = do
    a <- arbitrary
    return (return a)

-- | Retrieve the current `MockWorld`.
getMockWorld :: MockIO s (MockWorld s)
getMockWorld = MockIO $ \s -> (s,s)

-- | Replace the current `MockWorld`.
putMockWorld :: MockWorld s -> MockIO s ()
putMockWorld s = MockIO $ \_ -> ((),s)

-- | Mutate the current `MockWorld` strictly.
modifyMockWorld :: (MockWorld s -> MockWorld s) -> MockIO s ()
modifyMockWorld f = MockIO $ \s -> ((), f $! s)

-- | Bump the timer by a given number of microseconds.
incrementTimer :: Int -> MockIO s ()
incrementTimer k =
  modifyMockWorld $ \w -> w
    { _time = addUTCTime (fromIntegral k) $ _time w }



-- | Just enough state to mock out a basic filesystem and HTTP server.
data MockWorld s = MockWorld
  { _files :: FileSystem Handle
  , _time :: UTCTime

  , _httpGet :: String -> MockNetwork s HttpResponse
  , _httpPost :: String -> ByteString -> MockNetwork s HttpResponse
  , _httpDelete :: String -> MockNetwork s HttpResponse

  , _serverState :: MockServer s
  }

-- | Type representing the internal state of an HTTP server.
newtype MockServer s = MockServer
  { unMockServer :: s
  } deriving (Eq, Show)

-- | 1970-01-01 00:00:00
epoch :: UTCTime
epoch = UTCTime (ModifiedJulianDay 0) 0

-- | Empty filesystem and trivial HTTP responses
basicMockWorld :: s -> MockWorld s
basicMockWorld s = MockWorld
  { _files = emptyFileSystem
  , _time = epoch

  , _httpGet = \_ -> return $ _200ok "ok"
  , _httpPost = \_ _ -> return $ _200ok "ok"
  , _httpDelete = \_ -> return $ _200ok "ok"

  , _serverState = MockServer s
  }

instance (Eq s) => Eq (MockWorld s) where
  w1 == w2 = (_files w1 == _files w2)
    && (_serverState w1 == _serverState w2)

instance (Show s) => Show (MockWorld s) where
  show w = unlines
    [ "Filesystem:", "===========", show $ _files w
    , "Timestamp:", "==========", show $ _time w
    , "Server State:", "=============", show $ _serverState w
    ]

instance (Arbitrary s) => Arbitrary (MockWorld s) where
  arbitrary = basicMockWorld <$> arbitrary

instance (CoArbitrary s) => CoArbitrary (MockWorld s) where
  coarbitrary w =
    variant (diffTimeToPicoseconds $ utctDayTime $ _time w)



-- | State monad representing network interaction.
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

instance Show (MockNetwork s a) where
  show _ = "<MockNetwork>"

instance (Arbitrary a) => Arbitrary (MockNetwork s a) where
  arbitrary = do
    a <- arbitrary
    return (return a)

-- | Throw an `HttpException`.
errorMockNetwork :: HttpException -> MockNetwork s a
errorMockNetwork e = MockNetwork $ \s -> (Left e, s)

-- | Retrieve the internal state of the fake HTTP server.
getMockServer :: MockNetwork s s
getMockServer = MockNetwork $ \s -> (Right $ unMockServer s,s)

-- | Replace the internal state of the fake HTTP server.
putMockServer :: s -> MockNetwork s ()
putMockServer s = MockNetwork $ \_ -> (Right (), MockServer s)

-- | Mutate the internal state of the fake HTTP server (strictly).
modifyMockServer :: (s -> s) -> MockNetwork s ()
modifyMockServer f = MockNetwork $ \s ->
  (Right (), MockServer . f . unMockServer $! s)
