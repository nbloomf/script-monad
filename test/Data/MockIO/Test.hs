{-# LANGUAGE OverloadedStrings #-}
module Data.MockIO.Test (
    tests
) where

import Data.Proxy

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.QuickCheck.Laws

import Data.MockIO.FileSystem
import Control.Monad.Script.Http
import Data.MockIO



tests :: Int -> TestTree
tests num =
  localOption (QuickCheckTests $ 50 * num) $
  testGroup "Mock"
    [ testGroup "MockIO"
      [ testGroup "Monad Laws"
        [ testMonadLaws3 (pMockIO pU) pU pU pB pI mockIOEq
        , testMonadLaws3 (pMockIO pB) pB pU pB pI mockIOEq
        , testMonadLaws3 (pMockIO pI) pI pU pB pI mockIOEq
        ]
      , testGroup "Applicative Laws"
        [ testApplicativeLaws3 (pMockIO pU) pU pU pB pI mockIOEq
        , testApplicativeLaws3 (pMockIO pB) pB pU pB pI mockIOEq
        , testApplicativeLaws3 (pMockIO pI) pI pU pB pI mockIOEq
        ]
      , testGroup "Functor Laws"
        [ testFunctorLaws3 (pMockIO pU) pU pU pB pI mockIOEq
        , testFunctorLaws3 (pMockIO pB) pB pU pB pI mockIOEq
        , testFunctorLaws3 (pMockIO pI) pI pU pB pI mockIOEq
        ]
      ]
    , testGroup "MockNet"
      [ testGroup "Monad Laws"
        [ testMonadLaws3 (pMockNet pU) pU pU pB pI mockNetEq
        , testMonadLaws3 (pMockNet pB) pB pU pB pI mockNetEq
        , testMonadLaws3 (pMockNet pI) pI pU pB pI mockNetEq
        ]
      , testGroup "Applicative Laws"
        [ testApplicativeLaws3 (pMockNet pU) pU pU pB pI mockNetEq
        , testApplicativeLaws3 (pMockNet pB) pB pU pB pI mockNetEq
        , testApplicativeLaws3 (pMockNet pI) pI pU pB pI mockNetEq
        ]
      , testGroup "Functor Laws"
        [ testFunctorLaws3 (pMockNet pU) pU pU pB pI mockNetEq
        , testFunctorLaws3 (pMockNet pB) pB pU pB pI mockNetEq
        , testFunctorLaws3 (pMockNet pI) pI pU pB pI mockNetEq
        ]
      ]
    ]



pU = Proxy :: Proxy ()
pB = Proxy :: Proxy Bool
pI = Proxy :: Proxy Int

pMockIO :: Proxy s -> Proxy (MockIO s)
pMockIO _ = Proxy

pMockWorld :: Proxy s -> Proxy (MockWorld s)
pMockWorld _ = Proxy

pMockNet :: Proxy s -> Proxy (MockNetwork s)
pMockNet _ = Proxy



mockIOEq
  :: (Eq s, Eq a)
  => s
  -> MockIO s a
  -> MockIO s a
  -> Bool
mockIOEq s x y =
  (==)
    (runMockIO x $ basicMockWorld s)
    (runMockIO y $ basicMockWorld s)

mockNetEq
  :: (Eq s, Eq a)
  => s
  -> MockNetwork s a
  -> MockNetwork s a
  -> Bool
mockNetEq s x y =
  let
    (r1,s1) = unMockNetwork x $ MockServer s
    (r2,s2) = unMockNetwork y $ MockServer s
  in
    case (r1,r2) of
      (Right a1, Right a2) -> a1 == a2 && s1 == s2
      (Left _, Left _) -> s1 == s2
      _ -> False
