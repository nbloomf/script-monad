{-# LANGUAGE OverloadedStrings #-}
module Data.MockIO.Test.Server (
  simpleMockWorld
) where

import Data.MockIO
import Data.MockIO.FileSystem

simpleMockWorld :: MockWorld ()
simpleMockWorld = MockWorld
  { _files = emptyFileSystem
  , _time = epoch
  , _serverState = MockServer ()

  , _httpGet = \url -> case url of
      "http://example.com" -> return $ _200ok
        "welcome to example.com"

      "http://example.com/json" -> return $ _200ok
        "{\"key\":\"value\"}"

  , _httpPost = \url payload -> case url of
      "http://example.com" -> return $ _200ok
        "welcome to example.com"

  , _httpDelete = \url -> case url of
      "http://example.com" -> return $ _200ok
        "welcome to example.com"
  }
