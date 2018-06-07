{-# LANGUAGE OverloadedStrings #-}
module Control.Monad.Script.Http.Test (
    tests
) where

import Control.Concurrent.MVar
import System.IO

import Test.Tasty
import Test.Tasty.HUnit


import Control.Monad.Script.Http
import Control.Monad.Script.Http.MockIO

import Control.Monad.Script.Http.MockIO.Test.Server


tests :: MVar () -> TestTree
tests lock = testGroup "Http"
  [ testGroup "Real IO" $
      map (checkRealIO testState (testEnv lock)) theTestCases
  , testGroup "Mock IO" $
      map (checkMockIO testState (testEnv lock) basicMockWorld) theTestCases
  ]



testState = basicState ()

testEnv :: MVar () -> R () () ()
testEnv lock = env
  { _logOptions = basicLogOptions
    { _logSilent = True
    }
  }
  where
    env = basicEnv lock () :: R () () ()



checkRealIO
  :: S s
  -> R e w r
  -> ( String
     , String
     , Http e r w s Id t
     , (Either (E e) t, S s, W e w) -> Maybe (Either (E ()) ()) )
  -> TestTree
checkRealIO st env (name, msg, test, check) =
  testCase name $
    isOk msg $
    checkHttpM st env (evalIO evalId) id check test

checkMockIO
  :: S s
  -> R e w r
  -> MockWorld u
  -> ( String
     , String
     , Http e r w s Id t
     , (Either (E e) t, S s, W e w) -> Maybe (Either (E e) t) )
  -> TestTree
checkMockIO st env world (name, msg, test, check) =
  testCase name $
    isOk msg $
    checkHttpM st env (evalMockIO evalId) (toIO world) check test

isOk :: String -> IO (Maybe a) -> Assertion
isOk msg x = do
  p <- x
  case p of
    Nothing -> return ()
    Just _ -> assertFailure msg

data Id a = Id a

evalId :: (Monad m) => Id a -> m a
evalId (Id a) = return a

toIO :: MockWorld s -> MockIO s a -> IO a
toIO u (MockIO x) = do
  let (a,_) = x u
  return a



theTestCases
  :: [ ( String
       , String
       , Http () () () () Id ()
       , (Either (E ()) (), S (), W () ()) -> Maybe (Either (E ()) ())
       ) ]
theTestCases =
  [ ( "wait"
    , ""
    , wait 1
    , hasValueWith (== ())
    )

  , ( "comment"
    , ""
    , comment "foo"
    , hasValueWith (== ())
    )

  , ( "get"
    , ""
    , httpGet "http://example.com" >> return ()
    , hasValueWith (== ())
    )
  ]
