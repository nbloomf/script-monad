module Main where

import Control.Concurrent.MVar
import Test.Tasty
import System.Environment

import Data.MockIO.FileSystem.Test
import Control.Monad.Script.Test
import Control.Monad.Script.Http.Test
import Data.MockIO.Test

main :: IO ()
main = do
  k <- numTests
  setEnv "TASTY_NUM_THREADS" "3"
  lock <- newMVar ()
  defaultMain $
    testGroup "All Tests"
      [ Control.Monad.Script.Test.tests k
      , Control.Monad.Script.Http.Test.tests k lock
      , Data.MockIO.Test.tests k
      , Data.MockIO.FileSystem.Test.tests k
      ]

numTests :: IO Int
numTests = do
  var <- lookupEnv "CI"
  case var of
    Just "true" -> return 1
    _ -> return 10
