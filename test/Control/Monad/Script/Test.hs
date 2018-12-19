{-# LANGUAGE Rank2Types #-}
module Control.Monad.Script.Test (
  tests
) where

import Control.Monad.Trans.Identity
import Data.Proxy
import Data.Functor.Classes
import Data.Functor.Identity

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.QuickCheck.Laws

import Control.Monad.Script



tests :: Int -> TestTree
tests num =
  localOption (QuickCheckTests $ 10 * num) $
  testGroup "Script Properties"
    [ testGroup "Monad Laws"
      [ testMonadLaws3 (pSc pU pU pU pU pQ pId) (pSt pU pU) pU pB pI (scriptEq evalQ)
      , testMonadLaws3 (pSc pU pU pU pU pQ pMb) (pSt pU pU) pU pB pI (scriptEq evalQ)
      , testMonadLaws3 (pSc pU pU pU pU pQ pEi) (pSt pU pU) pU pB pI (scriptEq evalQ)
      , testMonadLaws3 (pSc pU pU pU pU pQ pLs) (pSt pU pU) pU pB pI (scriptEq evalQ)
      , testMonadLaws3 (pSc pU pS pU pS pQ pId) (pSt pS pS) pU pB pI (scriptEq evalQ)
      , testMonadLaws3 (pSc pU pS pU pS pQ pMb) (pSt pS pS) pU pB pI (scriptEq evalQ)
      , testMonadLaws3 (pSc pU pS pU pS pQ pEi) (pSt pS pS) pU pB pI (scriptEq evalQ)
      , testMonadLaws3 (pSc pU pS pU pS pQ pLs) (pSt pS pS) pU pB pI (scriptEq evalQ)
      ]

    , testGroup "Applicative Laws"
      [ testApplicativeLaws3 (pSc pU pU pU pU pQ pId) (pSt pU pU) pU pB pI (scriptEq evalQ)
      , testApplicativeLaws3 (pSc pU pU pU pU pQ pMb) (pSt pU pU) pU pB pI (scriptEq evalQ)
      , testApplicativeLaws3 (pSc pU pU pU pU pQ pEi) (pSt pU pU) pU pB pI (scriptEq evalQ)
      , testApplicativeLaws3 (pSc pU pU pU pU pQ pLs) (pSt pU pU) pU pB pI (scriptEq evalQ)
      , testApplicativeLaws3 (pSc pU pS pU pS pQ pId) (pSt pS pS) pU pB pI (scriptEq evalQ)
      , testApplicativeLaws3 (pSc pU pS pU pS pQ pMb) (pSt pS pS) pU pB pI (scriptEq evalQ)
      , testApplicativeLaws3 (pSc pU pS pU pS pQ pEi) (pSt pS pS) pU pB pI (scriptEq evalQ)
      , testApplicativeLaws3 (pSc pU pS pU pS pQ pLs) (pSt pS pS) pU pB pI (scriptEq evalQ)
      ]

    , testGroup "Functor Laws"
      [ testFunctorLaws3 (pSc pU pU pU pU pQ pId) (pSt pU pU) pU pB pI (scriptEq evalQ)
      , testFunctorLaws3 (pSc pU pU pU pU pQ pMb) (pSt pU pU) pU pB pI (scriptEq evalQ)
      , testFunctorLaws3 (pSc pU pU pU pU pQ pEi) (pSt pU pU) pU pB pI (scriptEq evalQ)
      , testFunctorLaws3 (pSc pU pU pU pU pQ pLs) (pSt pU pU) pU pB pI (scriptEq evalQ)
      , testFunctorLaws3 (pSc pU pS pU pS pQ pId) (pSt pS pS) pU pB pI (scriptEq evalQ)
      , testFunctorLaws3 (pSc pU pS pU pS pQ pMb) (pSt pS pS) pU pB pI (scriptEq evalQ)
      , testFunctorLaws3 (pSc pU pS pU pS pQ pEi) (pSt pS pS) pU pB pI (scriptEq evalQ)
      , testFunctorLaws3 (pSc pU pS pU pS pQ pLs) (pSt pS pS) pU pB pI (scriptEq evalQ)
      ]

    , testGroup "State Laws"
      [ testStateMonadLaws (pSc pU pU pU pU pQ pId) (pSt pU pU) pU pI (scriptEq evalQ) get put
      , testStateMonadLaws (pSc pU pU pU pU pQ pMb) (pSt pU pU) pU pI (scriptEq evalQ) get put
      , testStateMonadLaws (pSc pU pU pU pU pQ pEi) (pSt pU pU) pU pI (scriptEq evalQ) get put
      , testStateMonadLaws (pSc pU pU pU pU pQ pLs) (pSt pU pU) pU pI (scriptEq evalQ) get put
      , testStateMonadLaws (pSc pU pS pU pS pQ pId) (pSt pS pS) pS pI (scriptEq evalQ) get put
      , testStateMonadLaws (pSc pU pS pU pS pQ pMb) (pSt pS pS) pS pI (scriptEq evalQ) get put
      , testStateMonadLaws (pSc pU pS pU pS pQ pEi) (pSt pS pS) pS pI (scriptEq evalQ) get put
      , testStateMonadLaws (pSc pU pS pU pS pQ pLs) (pSt pS pS) pS pI (scriptEq evalQ) get put
      ]

    , testGroup "Reader Laws"
      [ testReaderMonadLaws (pSc pU pU pU pU pQ pId) (pSt pU pU) pU pU pB (scriptEq evalQ) ask local
      , testReaderMonadLaws (pSc pU pU pU pU pQ pMb) (pSt pU pU) pU pU pB (scriptEq evalQ) ask local
      , testReaderMonadLaws (pSc pU pU pU pU pQ pEi) (pSt pU pU) pU pU pB (scriptEq evalQ) ask local
      , testReaderMonadLaws (pSc pU pU pU pU pQ pLs) (pSt pU pU) pU pU pB (scriptEq evalQ) ask local
      , testReaderMonadLaws (pSc pU pS pU pS pQ pId) (pSt pS pS) pS pU pB (scriptEq evalQ) ask local
      , testReaderMonadLaws (pSc pU pS pU pS pQ pMb) (pSt pS pS) pS pU pB (scriptEq evalQ) ask local
      , testReaderMonadLaws (pSc pU pS pU pS pQ pEi) (pSt pS pS) pS pU pB (scriptEq evalQ) ask local
      , testReaderMonadLaws (pSc pU pS pU pS pQ pLs) (pSt pS pS) pS pU pB (scriptEq evalQ) ask local
      ]

    , testGroup "Writer"
      [ testGroup "Writer Laws"
        [ testWriterMonadLaws (pSc pU pU pU pU pQ pId) (pSt pU pU) pU pU pB (scriptEq evalQ) tell draft
        , testWriterMonadLaws (pSc pU pU pU pU pQ pMb) (pSt pU pU) pU pU pB (scriptEq evalQ) tell draft
        , testWriterMonadLaws (pSc pU pU pU pU pQ pEi) (pSt pU pU) pU pU pB (scriptEq evalQ) tell draft
        , testWriterMonadLaws (pSc pU pU pU pU pQ pLs) (pSt pU pU) pU pU pB (scriptEq evalQ) tell draft
        , testWriterMonadLaws (pSc pU pS pS pS pQ pId) (pSt pS pS) pS pU pB (scriptEq evalQ) tell draft
        , testWriterMonadLaws (pSc pU pS pS pS pQ pMb) (pSt pS pS) pS pU pB (scriptEq evalQ) tell draft
        , testWriterMonadLaws (pSc pU pS pS pS pQ pEi) (pSt pS pS) pS pU pB (scriptEq evalQ) tell draft
        , testWriterMonadLaws (pSc pU pS pS pS pQ pLs) (pSt pS pS) pS pU pB (scriptEq evalQ) tell draft
        ]

      , testGroup "Writer Equivalences"
        [ testWriterMonadEquivalences (pSc pU pU pU pU pQ pId) (pSt pU pU) pU pB (scriptEq evalQ) tell draft listen pass
        , testWriterMonadEquivalences (pSc pU pU pU pU pQ pMb) (pSt pU pU) pU pB (scriptEq evalQ) tell draft listen pass
        , testWriterMonadEquivalences (pSc pU pU pU pU pQ pEi) (pSt pU pU) pU pB (scriptEq evalQ) tell draft listen pass
        , testWriterMonadEquivalences (pSc pU pU pU pU pQ pLs) (pSt pU pU) pU pB (scriptEq evalQ) tell draft listen pass
        , testWriterMonadEquivalences (pSc pU pS pS pS pQ pId) (pSt pS pS) pS pB (scriptEq evalQ) tell draft listen pass
        , testWriterMonadEquivalences (pSc pU pS pS pS pQ pMb) (pSt pS pS) pS pB (scriptEq evalQ) tell draft listen pass
        , testWriterMonadEquivalences (pSc pU pS pS pS pQ pEi) (pSt pS pS) pS pB (scriptEq evalQ) tell draft listen pass
        , testWriterMonadEquivalences (pSc pU pS pS pS pQ pLs) (pSt pS pS) pS pB (scriptEq evalQ) tell draft listen pass
        ]
      ]

    , testGroup "Error Laws"
      [ testErrorMonadLaws (pSc pU pU pU pU pQ pId) (pSt pU pU) pU pB pI (scriptEq evalQ) throw catch
      , testErrorMonadLaws (pSc pU pU pU pU pQ pMb) (pSt pU pU) pU pB pI (scriptEq evalQ) throw catch
      , testErrorMonadLaws (pSc pU pU pU pU pQ pEi) (pSt pU pU) pU pB pI (scriptEq evalQ) throw catch
      , testErrorMonadLaws (pSc pU pU pU pU pQ pLs) (pSt pU pU) pU pB pI (scriptEq evalQ) throw catch
      , testErrorMonadLaws (pSc pS pS pU pS pQ pId) (pSt pS pS) pS pB pI (scriptEq evalQ) throw catch
      , testErrorMonadLaws (pSc pS pS pU pS pQ pMb) (pSt pS pS) pS pB pI (scriptEq evalQ) throw catch
      , testErrorMonadLaws (pSc pS pS pU pS pQ pEi) (pSt pS pS) pS pB pI (scriptEq evalQ) throw catch
      , testErrorMonadLaws (pSc pS pS pU pS pQ pLs) (pSt pS pS) pS pB pI (scriptEq evalQ) throw catch
      ]
    ]



-- | `ScriptTT` values are pure, so we can test them for equality.
scriptEq
  :: (Monad eff, Eq a, Eq e, Eq s, Eq w, Eq1 eff)
  => (forall u. p u -> eff u)
  -> (s, r)
  -> ScriptTT e r w s p IdentityT eff a
  -> ScriptTT e r w s p IdentityT eff a
  -> Bool
scriptEq eval (s,r) sc1 sc2 =
  liftEq (==)
    (execScriptTT s r eval sc1)
    (execScriptTT s r eval sc2)



data Q a = Q a

evalQ :: (Monad eff) => Q a -> eff a
evalQ (Q a) = return a

pQ = Proxy :: Proxy Q

pU = Proxy :: Proxy ()
pB = Proxy :: Proxy Bool
pI = Proxy :: Proxy Int
pS = Proxy :: Proxy String

pId = Proxy :: Proxy Identity
pMb = Proxy :: Proxy Maybe
pLs = Proxy :: Proxy []
pEi = Proxy :: Proxy (Either Int)

pSc
  :: Proxy e -> Proxy r -> Proxy w -> Proxy s -> Proxy p -> Proxy eff
  -> Proxy (ScriptTT e r w s p IdentityT eff)
pSc _ _ _ _ _ _ = Proxy

pSt
  :: Proxy s -> Proxy r
  -> Proxy (s,r)
pSt _ _ = Proxy
