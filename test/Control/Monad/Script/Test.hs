{-# LANGUAGE Rank2Types #-}
module Control.Monad.Script.Test (
  tests
) where

import Data.Proxy
import Data.Typeable
import Text.Show.Functions

import Test.Tasty
import Test.Tasty.QuickCheck

import Control.Monad.Script

tests :: TestTree
tests = testGroup "Script"
  [ test_monad_laws
  ]



scriptEq
  :: (Eq a, Eq e, Eq s, Eq w)
  => s
  -> r
  -> (forall u. p u -> u)
  -> Script e r w s p a
  -> Script e r w s p a
  -> Bool
scriptEq s r eval sc1 sc2 =
  (execScript s r eval sc1) == (execScript s r eval sc2)



prop_right_identity_law
  :: (Monoid w, Eq a, Eq e, Eq s, Eq w)
  => Proxy e -> Proxy r -> Proxy w -> Proxy s -> Proxy p -> Proxy a
  -> (forall u. p u -> u)
  -> s -> r -> Script e r w s p a -> Bool
prop_right_identity_law _ _ _ _ _ _ eval s r x =
  scriptEq s r eval (x >>= return) x


prop_left_identity_law
  :: (Monoid w, Eq b, Eq e, Eq s, Eq w)
  => Proxy e -> Proxy r -> Proxy w -> Proxy s -> Proxy p -> Proxy a -> Proxy b
  -> (forall u. p u -> u)
  -> s -> r -> a -> (a -> Script e r w s p b) -> Bool
prop_left_identity_law _ _ _ _ _ _ _ eval s r a f =
  scriptEq s r eval (return a >>= f) (f a)


prop_associativity_law
  :: (Monoid w, Eq c, Eq e, Eq s, Eq w)
  => Proxy e -> Proxy r -> Proxy w -> Proxy s -> Proxy p -> Proxy a -> Proxy b -> Proxy c
  -> (forall u. p u -> u)
  -> s -> r -> Script e r w s p a -> (a -> Script e r w s p b) -> (b -> Script e r w s p c) -> Bool
prop_associativity_law _ _ _ _ _ _ _ _ eval s r x f g =
  scriptEq s r eval ((x >>= f) >>= g) (x >>= (\z -> f z >>= g))



test_monad_laws_for
  :: ( Monoid w, Eq a, Eq b, Eq c, Eq e, Eq s, Eq w, Show s, Show r, Show a
     , Arbitrary s, Arbitrary r, Arbitrary a, Arbitrary b, Arbitrary c
     , CoArbitrary a, CoArbitrary b, CoArbitrary c
     , Typeable a, Typeable b, Typeable c, Typeable e
     , Typeable r, Typeable w, Typeable s, Typeable p
     )
  => Proxy e -> Proxy r -> Proxy w -> Proxy s -> Proxy p
  -> Proxy a -> Proxy b -> Proxy c
  -> (forall u. p u -> u)
  -> TestTree
test_monad_laws_for pe pr pw ps pp pa pb pc eval =
  let
    label = "Script " ++
      show (typeRep pe) ++ " " ++
      show (typeRep pr) ++ " " ++
      show (typeRep pw) ++ " " ++
      show (typeRep ps) ++ " " ++
      show (typeRep pp) ++ ", " ++
      show (typeRep pa) ++ ", " ++
      show (typeRep pb) ++ ", " ++
      show (typeRep pc) 
  in
    testGroup label $
      [ testProperty "right identity law" $
          prop_right_identity_law pe pr pw ps pp pa eval
      , testProperty "left identity law" $
          prop_left_identity_law pe pr pw ps pp pa pb eval
      , testProperty "associativity law" $
          prop_associativity_law pe pr pw ps pp pa pb pc eval
      ]



test_monad_laws :: TestTree
test_monad_laws = testGroup "Monad Laws"
  [ test_monad_laws_for pU pU pU pU pQ pU pU pU evalQ
  , test_monad_laws_for pU pU pU pU pQ pB pB pB evalQ
  , test_monad_laws_for pU pU pU pU pQ pI pI pI evalQ
  , test_monad_laws_for pU pU pU pU pQ pI pI pB evalQ
  , test_monad_laws_for pU pU pU pU pQ pI pB pI evalQ
  , test_monad_laws_for pU pU pU pU pQ pB pI pI evalQ
  , test_monad_laws_for pU pU pU pU pQ pS pS pS evalQ
  , test_monad_laws_for pU pU pU pU pQ pS pS pI evalQ
  , test_monad_laws_for pU pU pU pU pQ pS pI pS evalQ
  , test_monad_laws_for pU pU pU pU pQ pI pS pS evalQ

  , test_monad_laws_for pS pS pS pS pQ pU pU pU evalQ
  , test_monad_laws_for pS pS pS pS pQ pB pB pB evalQ
  , test_monad_laws_for pS pS pS pS pQ pI pI pI evalQ
  , test_monad_laws_for pS pS pS pS pQ pI pI pB evalQ
  , test_monad_laws_for pS pS pS pS pQ pI pB pI evalQ
  , test_monad_laws_for pS pS pS pS pQ pB pI pI evalQ
  , test_monad_laws_for pS pS pS pS pQ pS pS pS evalQ
  , test_monad_laws_for pS pS pS pS pQ pS pS pI evalQ
  , test_monad_laws_for pS pS pS pS pQ pS pI pS evalQ
  , test_monad_laws_for pS pS pS pS pQ pI pS pS evalQ
  ]



data Q a = Q a

evalQ :: Q a -> a
evalQ (Q a) = a

pQ :: Proxy Q
pQ = Proxy

pU :: Proxy ()
pU = Proxy

pB :: Proxy Bool
pB = Proxy

pI :: Proxy Int
pI = Proxy

pS :: Proxy String
pS = Proxy
