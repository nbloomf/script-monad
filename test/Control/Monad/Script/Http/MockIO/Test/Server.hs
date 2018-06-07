{-# LANGUAGE OverloadedStrings #-}
module Control.Monad.Script.Http.MockIO.Test.Server (
  basicMockWorld
) where

import Control.Monad.Script.Http.MockIO

basicMockWorld :: MockWorld ()
basicMockWorld = MockWorld
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
