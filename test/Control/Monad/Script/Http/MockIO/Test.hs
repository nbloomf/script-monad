{-# LANGUAGE OverloadedStrings #-}
module Control.Monad.Script.Http.MockIO.Test (
    tests
) where

import Prelude hiding (lookup)
import Data.ByteString.Lazy (ByteString)
import Control.Exception (SomeException)

import Test.Tasty
import Test.Tasty.QuickCheck

import Control.Monad.Script.Http
import Control.Monad.Script.Http.MockIO

tests :: TestTree
tests = testGroup "Http: MockIO"
  [ 
  ]


test_http_get :: Http () () () () Id ()
test_http_get = do
  response <- httpGet "http://example.com"
  return ()

test_error :: Http () () () () Id ()
test_error = undefined

data Id x = Id x

evalId :: (Monad m) => Id x -> m x
evalId (Id x) = return x





mockUnitId
  :: Http () () () () Id t
  -> ((Either (E ()) t, S (), W () ()), MockWorld ())
mockUnitId = execHttpMockIO
  (basicState ())
  (basicEnv undefined ())
  evalId
  theMockWorld


theMockWorld :: MockWorld ()
theMockWorld = MockWorld
  { _files = []
  , _time = epoch
  , _serverState = MockServer ()

  , _httpGet = \url -> case url of
      "http://example.com" -> return $ _200ok
        "welcome to example.com"

  , _httpPost = \url payload -> case url of
      "http://example.com" -> return $ _200ok
        "welcome to example.com"

  , _httpDelete = \url -> case url of
      "http://example.com" -> return $ _200ok
        "welcome to example.com"
  }
