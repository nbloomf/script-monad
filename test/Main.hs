module Main where

import Control.Concurrent.MVar
import Test.Tasty

import Control.Monad.Script.Test
import Control.Monad.Script.Http.Test
import Control.Monad.Script.Http.MockIO.Test

main :: IO ()
main = do
  lock <- newMVar ()
  defaultMain $
    testGroup "All Tests"
      [ Control.Monad.Script.Test.tests
      , Control.Monad.Script.Http.Test.tests lock
      , Control.Monad.Script.Http.MockIO.Test.tests
      ]
