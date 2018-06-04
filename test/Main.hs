module Main where

import Test.Tasty

import Control.Monad.Script.Test

main :: IO ()
main = defaultMain $
  testGroup "All Tests"
    [ Control.Monad.Script.Test.tests
    ]
