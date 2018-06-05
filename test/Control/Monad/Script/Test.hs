{-# LANGUAGE Rank2Types, ScopedTypeVariables #-}
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



as :: a -> a -> a
as _ a = a



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
  => Proxy e -> Proxy r -> Proxy w
  -> Proxy s -> Proxy p -> Proxy a
  -> (forall u. p u -> u)
  -> s -> r -> Script e r w s p a -> Bool
prop_right_identity_law _ _ _ _ _ _ eval s r x =
  scriptEq s r eval (x >>= return) x



prop_left_identity_law
  :: (Monoid w, Eq b, Eq e, Eq s, Eq w)
  => Proxy e -> Proxy r -> Proxy w
  -> Proxy s -> Proxy p -> Proxy a -> Proxy b
  -> (forall u. p u -> u)
  -> s -> r -> a -> (a -> Script e r w s p b) -> Bool
prop_left_identity_law _ _ _ _ _ _ _ eval s r a f =
  scriptEq s r eval (return a >>= f) (f a)



prop_associativity_law
  :: (Monoid w, Eq c, Eq e, Eq s, Eq w)
  => Proxy e -> Proxy r -> Proxy w -> Proxy s
  -> Proxy p -> Proxy a -> Proxy b -> Proxy c
  -> (forall u. p u -> u)
  -> s -> r
  -> Script e r w s p a
  -> (a -> Script e r w s p b)
  -> (b -> Script e r w s p c)
  -> Bool
prop_associativity_law _ _ _ _ _ _ _ _ eval s r x f g =
  scriptEq s r eval ((x >>= f) >>= g) (x >>= (\z -> f z >>= g))



prop_tell_hom_law
  :: (Monoid w, Eq e, Eq s, Eq w)
  => Proxy e -> Proxy r -> Proxy w
  -> Proxy s -> Proxy p
  -> (forall u. p u -> u)
  -> s -> r -> w -> w -> Script e r w s p () -> Bool
prop_tell_hom_law _ _ _ _ _ eval s r w1 w2 x =
  scriptEq s r eval
    (as x (tell w1) >> as x (tell w2))
    (as x (tell (mappend w1 w2)))



prop_listen_tell_law
  :: (Monoid w, Eq e, Eq s, Eq w)
  => Proxy e -> Proxy r -> Proxy w
  -> Proxy s -> Proxy p
  -> (forall u. p u -> u)
  -> s -> r -> w -> Script e r w s p () -> Bool
prop_listen_tell_law _ _ _ _ _ eval s r w x =
  scriptEq s r eval
    (listen (as x (tell w)))
    ((as x (tell w)) >> return ((),w))



prop_censor_tell_law
  :: (Monoid w, Eq e, Eq s, Eq w)
  => Proxy e -> Proxy r -> Proxy w
  -> Proxy s -> Proxy p
  -> (forall u. p u -> u)
  -> s -> r -> (w -> w) -> w -> Script e r w s p () -> Bool
prop_censor_tell_law _ _ _ _ _ eval s r f w x =
  scriptEq s r eval
    (censor f (as x (tell w)))
    (as x (tell (f w)))



prop_get_put_law
  :: (Monoid w, Eq e, Eq s, Eq w)
  => Proxy e -> Proxy r -> Proxy w
  -> Proxy s -> Proxy p
  -> (forall u. p u -> u)
  -> s -> r -> Script e r w s p s -> Bool
prop_get_put_law _ _ _ _ _ eval s r x =
  scriptEq s r eval
    ((as x get) >>= put)
    (return ())



prop_put_put_law
  :: (Monoid w, Eq e, Eq s, Eq w)
  => Proxy e -> Proxy r -> Proxy w
  -> Proxy s -> Proxy p
  -> (forall u. p u -> u)
  -> s -> r -> s -> s -> Script e r w s p () -> Bool
prop_put_put_law _ _ _ _ _ eval s r u v x =
  scriptEq s r eval
    ((as x (put u)) >> put v)
    (as x (put v))



prop_put_get_law
  :: (Monoid w, Eq e, Eq s, Eq w)
  => Proxy e -> Proxy r -> Proxy w
  -> Proxy s -> Proxy p
  -> (forall u. p u -> u)
  -> s -> r -> s -> Script e r w s p () -> Bool
prop_put_get_law _ _ _ _ _ eval s r u x =
  scriptEq s r eval
    ((as x (put u)) >> get)
    ((as x (put u)) >> return u)



test_script_properties_for
  :: ( Monoid w, Eq a, Eq b, Eq c, Eq e, Eq s, Eq w, Show s, Show r, Show a, Show w
     , Arbitrary s, Arbitrary r, Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary w
     , CoArbitrary a, CoArbitrary b, CoArbitrary c, CoArbitrary w, CoArbitrary s
     , Typeable a, Typeable b, Typeable c, Typeable e
     , Typeable r, Typeable w, Typeable s, Typeable p
     )
  => Proxy e -> Proxy r -> Proxy w -> Proxy s -> Proxy p
  -> Proxy a -> Proxy b -> Proxy c
  -> (forall u. p u -> u)
  -> TestTree
test_script_properties_for pe pr pw ps pp pa pb pc eval =
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
      [ testGroup "Monad Laws" $
          [ testProperty "x >>= return === x" $
            prop_right_identity_law pe pr pw ps pp pa eval
          , testProperty "return a >>= f === f a" $
            prop_left_identity_law pe pr pw ps pp pa pb eval
          , testProperty "(x >>= f) >>= g === x >>= (\\z -> f z >>= g)" $
            prop_associativity_law pe pr pw ps pp pa pb pc eval
          ]
      , testGroup "Writer Laws" $
          [ testProperty "tell u >> tell v === tell (mappend u v)" $
            prop_tell_hom_law pe pr pw ps pp eval
          , testProperty "listen (tell w) === tell w >> return ((),w)" $
            prop_listen_tell_law pe pr pw ps pp eval
          , testProperty "censor f (tell w) === tell (f w)" $
            prop_censor_tell_law pe pr pw ps pp eval
          ]
      , testGroup "State Laws" $
          [ testProperty "get >>= put === return ()" $
            prop_get_put_law pe pr pw ps pp eval
          , testProperty "put a >> put b === put b" $
            prop_put_put_law pe pr pw ps pp eval
          , testProperty "put s >> get === put s >> return s" $
            prop_put_get_law pe pr pw ps pp eval
          ]
      ]



test_monad_laws :: TestTree
test_monad_laws =
  localOption (QuickCheckTests 1000) $
  testGroup "Script properties"
    [ test_script_properties_for pU pU pU pU pQ pU pU pU evalQ
    , test_script_properties_for pU pU pU pU pQ pB pB pB evalQ
    , test_script_properties_for pU pU pU pU pQ pI pI pI evalQ
    , test_script_properties_for pU pU pU pU pQ pI pI pB evalQ
    , test_script_properties_for pU pU pU pU pQ pI pB pI evalQ
    , test_script_properties_for pU pU pU pU pQ pB pI pI evalQ
    , test_script_properties_for pU pU pU pU pQ pS pS pS evalQ
    , test_script_properties_for pU pU pU pU pQ pS pS pI evalQ
    , test_script_properties_for pU pU pU pU pQ pS pI pS evalQ
    , test_script_properties_for pU pU pU pU pQ pI pS pS evalQ

    , test_script_properties_for pS pS pS pS pQ pU pU pU evalQ
    , test_script_properties_for pS pS pS pS pQ pB pB pB evalQ
    , test_script_properties_for pS pS pS pS pQ pI pI pI evalQ
    , test_script_properties_for pS pS pS pS pQ pI pI pB evalQ
    , test_script_properties_for pS pS pS pS pQ pI pB pI evalQ
    , test_script_properties_for pS pS pS pS pQ pB pI pI evalQ
    , test_script_properties_for pS pS pS pS pQ pS pS pS evalQ
    , test_script_properties_for pS pS pS pS pQ pS pS pI evalQ
    , test_script_properties_for pS pS pS pS pQ pS pI pS evalQ
    , test_script_properties_for pS pS pS pS pQ pI pS pS evalQ
    ]



data Q a = Q a

evalQ :: Q a -> a
evalQ (Q a) = a

pQ = Proxy :: Proxy Q
pU = Proxy :: Proxy ()
pB = Proxy :: Proxy Bool
pI = Proxy :: Proxy Int
pS = Proxy :: Proxy String
