{-# LANGUAGE Rank2Types, ScopedTypeVariables #-}
module Control.Monad.Script.Test (
  tests
) where

import Data.Proxy
import Data.Functor.Classes
import Data.Functor.Identity
import Data.Typeable
import Text.Show.Functions

import Test.Tasty
import Test.Tasty.QuickCheck

import Control.Monad.Script

tests :: Int -> TestTree
tests num =
  localOption (QuickCheckTests $ 10 * num) $
  testGroup "Script"
    [ test_script_properties
    ]



as :: a -> a -> a
as _ a = a



scriptEq
  :: (Monad m, Eq a, Eq e, Eq s, Eq w, Eq1 m)
  => s
  -> r
  -> (forall u. p u -> u)
  -> ScriptT e r w s p m a
  -> ScriptT e r w s p m a
  -> Bool
scriptEq s r eval sc1 sc2 =
  liftEq (==)
    (execScriptT s r eval sc1)
    (execScriptT s r eval sc2)



prop_right_identity_law
  :: (Monad m, Monoid w, Eq a, Eq e, Eq s, Eq w, Eq1 m)
  => Proxy e -> Proxy r -> Proxy w -> Proxy s
  -> Proxy p -> Proxy a -> Proxy m
  -> (forall u. p u -> u)
  -> s -> r -> ScriptT e r w s p m a -> Bool
prop_right_identity_law _ _ _ _ _ _ _ eval s r x =
  scriptEq s r eval (x >>= return) x



prop_left_identity_law
  :: (Monad m, Monoid w, Eq b, Eq e, Eq s, Eq w, Eq1 m)
  => Proxy e -> Proxy r -> Proxy w
  -> Proxy s -> Proxy p -> Proxy a -> Proxy b -> Proxy m
  -> (forall u. p u -> u)
  -> s -> r -> a -> (a -> ScriptT e r w s p m b) -> Bool
prop_left_identity_law _ _ _ _ _ _ _ _ eval s r a f =
  scriptEq s r eval (return a >>= f) (f a)



prop_associativity_law
  :: (Monad m, Monoid w, Eq c, Eq e, Eq s, Eq w, Eq1 m)
  => Proxy e -> Proxy r -> Proxy w -> Proxy s
  -> Proxy p -> Proxy a -> Proxy b -> Proxy c -> Proxy m
  -> (forall u. p u -> u)
  -> s -> r
  -> ScriptT e r w s p m a
  -> (a -> ScriptT e r w s p m b)
  -> (b -> ScriptT e r w s p m c)
  -> Bool
prop_associativity_law _ _ _ _ _ _ _ _ _ eval s r x f g =
  scriptEq s r eval ((x >>= f) >>= g) (x >>= (\z -> f z >>= g))



prop_applicative_identity_law
  :: (Monad m, Monoid w, Eq a, Eq e, Eq s, Eq w, Eq1 m)
  => Proxy e -> Proxy r -> Proxy w
  -> Proxy s -> Proxy p -> Proxy a -> Proxy m
  -> (forall u. p u -> u)
  -> s -> r -> ScriptT e r w s p m a -> Bool
prop_applicative_identity_law _ _ _ _ _ _ _ eval s r x =
  scriptEq s r eval (pure id <*> x) x



prop_applicative_hom_law
  :: (Monad m, Monoid w, Eq a, Eq b, Eq e, Eq s, Eq w, Eq1 m)
  => Proxy e -> Proxy r -> Proxy w
  -> Proxy s -> Proxy p -> Proxy a -> Proxy b -> Proxy m
  -> (forall u. p u -> u)
  -> s -> r -> (a -> b) -> a -> ScriptT e r w s p m a -> Bool
prop_applicative_hom_law _ _ _ _ _ _ _ _ eval s r f a x =
  scriptEq s r eval
    (pure f <*> (as x $ pure a))
    (pure (f a))



prop_applicative_interchange_law
  :: (Monad m, Monoid w, Eq a, Eq b, Eq e, Eq s, Eq w, Eq1 m)
  => Proxy e -> Proxy r -> Proxy w
  -> Proxy s -> Proxy p -> Proxy a -> Proxy b -> Proxy m
  -> (forall u. p u -> u)
  -> s -> r
  -> ScriptT e r w s p m (a -> b)
  -> a
  -> ScriptT e r w s p m a -> Bool
prop_applicative_interchange_law _ _ _ _ _ _ _ _ eval s r u a x =
  scriptEq s r eval
    (u <*> (as x $ pure a))
    (pure ($ a) <*> u)



prop_applicative_composition_law
  :: (Monad m, Monoid w, Eq a, Eq b, Eq c, Eq e, Eq s, Eq w, Eq1 m)
  => Proxy e -> Proxy r -> Proxy w
  -> Proxy s -> Proxy p -> Proxy a -> Proxy b -> Proxy c -> Proxy m
  -> (forall u. p u -> u)
  -> s -> r
  -> ScriptT e r w s p m (a -> b)
  -> ScriptT e r w s p m (b -> c)
  -> ScriptT e r w s p m a
  -> Bool
prop_applicative_composition_law _ _ _ _ _ _ _ _ _ eval s r u v x =
  scriptEq s r eval
    (pure (.) <*> v <*> u <*> x)
    (v <*> (u <*> x))



prop_tell_hom_law
  :: (Monad m, Monoid w, Eq e, Eq s, Eq w, Eq1 m)
  => Proxy e -> Proxy r -> Proxy w
  -> Proxy s -> Proxy p -> Proxy m
  -> (forall u. p u -> u)
  -> s -> r -> w -> w -> ScriptT e r w s p m () -> Bool
prop_tell_hom_law _ _ _ _ _ _ eval s r w1 w2 x =
  scriptEq s r eval
    (as x (tell w1) >> as x (tell w2))
    (as x (tell (mappend w1 w2)))



prop_listen_tell_law
  :: (Monad m, Monoid w, Eq e, Eq s, Eq w, Eq1 m)
  => Proxy e -> Proxy r -> Proxy w
  -> Proxy s -> Proxy p -> Proxy m
  -> (forall u. p u -> u)
  -> s -> r -> w -> ScriptT e r w s p m () -> Bool
prop_listen_tell_law _ _ _ _ _ _ eval s r w x =
  scriptEq s r eval
    (listen (as x (tell w)))
    ((as x (tell w)) >> return ((),w))



prop_censor_tell_law
  :: (Monad m, Monoid w, Eq e, Eq s, Eq w, Eq1 m)
  => Proxy e -> Proxy r -> Proxy w
  -> Proxy s -> Proxy p -> Proxy m
  -> (forall u. p u -> u)
  -> s -> r -> (w -> w) -> w -> ScriptT e r w s p m () -> Bool
prop_censor_tell_law _ _ _ _ _ _ eval s r f w x =
  scriptEq s r eval
    (censor f (as x (tell w)))
    (as x (tell (f w)))



prop_get_put_law
  :: (Monad m, Monoid w, Eq e, Eq s, Eq w, Eq1 m)
  => Proxy e -> Proxy r -> Proxy w
  -> Proxy s -> Proxy p -> Proxy m
  -> (forall u. p u -> u)
  -> s -> r -> ScriptT e r w s p m s -> Bool
prop_get_put_law _ _ _ _ _ _ eval s r x =
  scriptEq s r eval
    ((as x get) >>= put)
    (return ())



prop_put_put_law
  :: (Monad m, Monoid w, Eq e, Eq s, Eq w, Eq1 m)
  => Proxy e -> Proxy r -> Proxy w
  -> Proxy s -> Proxy p -> Proxy m
  -> (forall u. p u -> u)
  -> s -> r -> s -> s -> ScriptT e r w s p m () -> Bool
prop_put_put_law _ _ _ _ _ _ eval s r u v x =
  scriptEq s r eval
    ((as x (put u)) >> put v)
    (as x (put v))



prop_put_get_law
  :: (Monad m, Monoid w, Eq e, Eq s, Eq w, Eq1 m)
  => Proxy e -> Proxy r -> Proxy w
  -> Proxy s -> Proxy p -> Proxy m
  -> (forall u. p u -> u)
  -> s -> r -> s -> ScriptT e r w s p m () -> Bool
prop_put_get_law _ _ _ _ _ _ eval s r u x =
  scriptEq s r eval
    ((as x (put u)) >> get)
    ((as x (put u)) >> return u)



prop_put_modify_law
  :: (Monad m, Monoid w, Eq e, Eq s, Eq w, Eq1 m)
  => Proxy e -> Proxy r -> Proxy w
  -> Proxy s -> Proxy p -> Proxy m
  -> (forall u. p u -> u)
  -> s -> r -> s -> (s -> s) -> ScriptT e r w s p m () -> Bool
prop_put_modify_law _ _ _ _ _ _ eval s r u f x =
  scriptEq s r eval
    ((as x (put u)) >> modify f)
    ((as x (put $ f u)))



prop_put_modify'_law
  :: (Monad m, Monoid w, Eq e, Eq s, Eq w, Eq1 m)
  => Proxy e -> Proxy r -> Proxy w
  -> Proxy s -> Proxy p -> Proxy m
  -> (forall u. p u -> u)
  -> s -> r -> s -> (s -> s) -> ScriptT e r w s p m () -> Bool
prop_put_modify'_law _ _ _ _ _ _ eval s r u f x =
  scriptEq s r eval
    ((as x (put u)) >> modify' f)
    ((as x (put $ f u)))



prop_gets_modify_law
  :: (Monad m, Monoid w, Eq e, Eq s, Eq a, Eq w, Eq1 m)
  => Proxy e -> Proxy r -> Proxy w
  -> Proxy s -> Proxy p -> Proxy m -> Proxy a
  -> (forall u. p u -> u)
  -> s -> r -> (s -> a) -> ScriptT e r w s p m a -> Bool
prop_gets_modify_law _ _ _ _ _ _ _ eval s r f x =
  scriptEq s r eval
    (gets f)
    (as x (f <$> get))



prop_reader_ask_law
  :: (Monad m, Monoid w, Eq e, Eq a, Eq s, Eq w, Eq1 m)
  => Proxy e -> Proxy r -> Proxy w
  -> Proxy s -> Proxy p -> Proxy m -> Proxy a
  -> (forall u. p u -> u)
  -> s -> r -> (r -> a) -> ScriptT e r w s p m a -> Bool
prop_reader_ask_law _ _ _ _ _ _ _ eval s r f x =
  scriptEq s r eval
    (reader f)
    (as x (f <$> ask))



prop_local_ask_law
  :: (Monad m, Monoid w, Eq e, Eq s, Eq r, Eq w, Eq1 m)
  => Proxy e -> Proxy r -> Proxy w
  -> Proxy s -> Proxy p -> Proxy m
  -> (forall u. p u -> u)
  -> s -> r -> (r -> r) -> ScriptT e r w s p m r -> Bool
prop_local_ask_law _ _ _ _ _ _ eval s r f x =
  scriptEq s r eval
    (as x (local f ask))
    (as x (f <$> ask))



prop_throw_catch_law
  :: (Monad m, Monoid w, Eq e, Eq s, Eq r, Eq w, Eq1 m)
  => Proxy e -> Proxy r -> Proxy w
  -> Proxy s -> Proxy p -> Proxy m
  -> (forall u. p u -> u)
  -> s -> r -> e -> ScriptT e r w s p m e -> Bool
prop_throw_catch_law _ _ _ _ _ _ eval s r e x =
  scriptEq s r eval
    (as x (return e))
    (as x (catch (throw e) return))



prop_catch_return_law
  :: (Monad m, Monoid w, Eq e, Eq a, Eq s, Eq r, Eq w, Eq1 m)
  => Proxy e -> Proxy r -> Proxy w
  -> Proxy s -> Proxy p -> Proxy a -> Proxy m
  -> (forall u. p u -> u)
  -> s -> r -> a -> ScriptT e r w s p m a -> Bool
prop_catch_return_law _ _ _ _ _ _ _ eval s r a x =
  scriptEq s r eval
    (as x (return a))
    (as x (catch (return a) undefined))



prop_throw_except_law
  :: (Monad m, Monoid w, Eq e, Eq s, Eq r, Eq a, Eq w, Eq1 m)
  => Proxy e -> Proxy r -> Proxy w
  -> Proxy s -> Proxy p -> Proxy m -> Proxy a
  -> (forall u. p u -> u)
  -> s -> r -> e -> ScriptT e r w s p m a -> Bool
prop_throw_except_law _ _ _ _ _ _ _ eval s r e x =
  scriptEq s r eval
    (as x (except $ Left e))
    (as x (throw e))



prop_return_except_law
  :: ( Monad m
     , Monoid w
     , Eq e, Eq s, Eq r, Eq a, Eq w
     , Eq1 m
     )
  => Proxy e -> Proxy r -> Proxy w
  -> Proxy s -> Proxy p -> Proxy m -> Proxy a
  -> (forall u. p u -> u)
  -> s -> r -> a -> ScriptT e r w s p m a -> Bool
prop_return_except_law _ _ _ _ _ _ _ eval s r a x =
  scriptEq s r eval
    (as x (except $ Right a))
    (as x (return a))



prop_throw_triage_law
  :: ( Monad m
     , Monoid w
     , Eq e, Eq s, Eq r, Eq a, Eq w
     , Eq1 m
     )
  => Proxy e -> Proxy r -> Proxy w
  -> Proxy s -> Proxy p -> Proxy m -> Proxy a
  -> (forall u. p u -> u)
  -> s -> r -> e -> (e -> e) -> ScriptT e r w s p m a -> Bool
prop_throw_triage_law _ _ _ _ _ _ _ eval s r e f x =
  scriptEq s r eval
    (as x (triage f $ throw e))
    (as x (throw (f e)))



test_script_properties_for
  :: ( Monad m
     , Monoid w
     , Eq a, Eq b, Eq c, Eq e, Eq s, Eq w, Eq r
     , Eq1 m
     , Show s, Show r, Show a, Show w, Show e
     , Arbitrary s, Arbitrary r, Arbitrary a, Arbitrary b
     , Arbitrary c, Arbitrary w, Arbitrary e
     , CoArbitrary a, CoArbitrary b, CoArbitrary c, CoArbitrary w
     , CoArbitrary s, CoArbitrary r, CoArbitrary e
     , Typeable a, Typeable b, Typeable c, Typeable e, Typeable r
     , Typeable w, Typeable s, Typeable p, Typeable m
     )
  => Proxy e -> Proxy r -> Proxy w -> Proxy s -> Proxy p
  -> Proxy a -> Proxy b -> Proxy c -> Proxy m
  -> (forall u. p u -> u)
  -> TestTree
test_script_properties_for pe pr pw ps pp pa pb pc pm eval =
  let
    label = "Script: " ++
      "E: " ++ show (typeRep pe) ++ " " ++
      "R: " ++ show (typeRep pr) ++ " " ++
      "W: " ++ show (typeRep pw) ++ " " ++
      "S: " ++ show (typeRep ps) ++ " " ++
      "P: " ++ show (typeRep pp) ++ ", " ++
      "a: " ++ show (typeRep pa) ++ ", " ++
      "b: " ++ show (typeRep pb) ++ ", " ++
      "c: " ++ show (typeRep pc) ++ ", " ++
      "m: " ++ show (typeRep pm)
  in
    testGroup label $
      [ testGroup "Monad Laws"
          [ testProperty "x >>= return === x" $
            prop_right_identity_law pe pr pw ps pp pa pm eval
          , testProperty "return a >>= f === f a" $
            prop_left_identity_law pe pr pw ps pp pa pb pm eval
          , testProperty "(x >>= f) >>= g === x >>= (\\z -> f z >>= g)" $
            prop_associativity_law pe pr pw ps pp pa pb pc pm eval
          ]
      , testGroup "Applicative Laws"
          [ testProperty "pure id <*> x === x" $
            prop_applicative_identity_law pe pr pw ps pp pa pm eval
          , testProperty "pure f <*> pure x === pure (f x)" $
            prop_applicative_hom_law pe pr pw ps pp pa pb pm eval
          , testProperty "u <*> pure x === pure ($ x) <*> u" $
            prop_applicative_interchange_law pe pr pw ps pp pa pb pm eval
          , testProperty "pure (.) <*> v <*> u <*> x === v <*> (u <*> x)" $
            prop_applicative_composition_law pe pr pw ps pp pa pb pc pm eval
          ]
      , testGroup "Writer Laws"
          [ testProperty "tell u >> tell v === tell (mappend u v)" $
            prop_tell_hom_law pe pr pw ps pp pm eval
          , testProperty "listen (tell w) === tell w >> return ((),w)" $
            prop_listen_tell_law pe pr pw ps pp pm eval
          , testProperty "censor f (tell w) === tell (f w)" $
            prop_censor_tell_law pe pr pw ps pp pm eval
          ]
      , testGroup "State Laws"
          [ testProperty "get >>= put === return ()" $
            prop_get_put_law pe pr pw ps pp pm eval
          , testProperty "put a >> put b === put b" $
            prop_put_put_law pe pr pw ps pp pm eval
          , testProperty "put s >> get === put s >> return s" $
            prop_put_get_law pe pr pw ps pp pm eval
          , testProperty "put s >> modify f === put (f s)" $
            prop_put_modify_law pe pr pw ps pp pm eval
          , testProperty "put s >> modify' f === put (f s)" $
            prop_put_modify'_law pe pr pw ps pp pm eval
          , testProperty "gets f === f <$> get" $
            prop_gets_modify_law pe pr pw ps pp pm pa eval
          ]
      , testGroup "Reader Laws"
          [ testProperty "reader f === f <$> ask" $
            prop_reader_ask_law pe pr pw ps pp pm pa eval
          , testProperty "local f ask === f <$> ask" $
            prop_local_ask_law pe pr pw ps pp pm eval
          ]
      , testGroup "Error Laws"
          [ testProperty "return e === catch (throw e) return" $
            prop_throw_catch_law pe pr pw ps pp pm eval
          , testProperty "except (Left e) === throw e" $
            prop_throw_except_law pe pr pw ps pp pm pa eval
          , testProperty "except (Right a) === return a" $
            prop_return_except_law pe pr pw ps pp pm pa eval
          , testProperty "catch (return a) undefined === return a" $
            prop_catch_return_law pe pr pw ps pp pa pm eval
          , testProperty "triage f (throw e) === throw (f e)" $
            prop_throw_triage_law pe pr pw ps pp pm pa eval
          ]
      ]



test_script_properties :: TestTree
test_script_properties =
  testGroup "Script Properties"
    -- Identity
    [ test_script_properties_for pU pU pU pU pQ pU pU pU pId evalQ
    , test_script_properties_for pU pU pU pU pQ pB pB pB pId evalQ
    , test_script_properties_for pU pU pU pU pQ pI pI pI pId evalQ
    , test_script_properties_for pU pU pU pU pQ pI pI pB pId evalQ
    , test_script_properties_for pU pU pU pU pQ pI pB pI pId evalQ
    , test_script_properties_for pU pU pU pU pQ pB pI pI pId evalQ
    , test_script_properties_for pU pU pU pU pQ pS pS pS pId evalQ
    , test_script_properties_for pU pU pU pU pQ pS pS pI pId evalQ
    , test_script_properties_for pU pU pU pU pQ pS pI pS pId evalQ
    , test_script_properties_for pU pU pU pU pQ pI pS pS pId evalQ

    , test_script_properties_for pS pS pS pS pQ pU pU pU pId evalQ
    , test_script_properties_for pS pS pS pS pQ pB pB pB pId evalQ
    , test_script_properties_for pS pS pS pS pQ pI pI pI pId evalQ
    , test_script_properties_for pS pS pS pS pQ pI pI pB pId evalQ
    , test_script_properties_for pS pS pS pS pQ pI pB pI pId evalQ
    , test_script_properties_for pS pS pS pS pQ pB pI pI pId evalQ
    , test_script_properties_for pS pS pS pS pQ pS pS pS pId evalQ
    , test_script_properties_for pS pS pS pS pQ pS pS pI pId evalQ
    , test_script_properties_for pS pS pS pS pQ pS pI pS pId evalQ
    , test_script_properties_for pS pS pS pS pQ pI pS pS pId evalQ

    -- Maybe
    , test_script_properties_for pU pU pU pU pQ pU pU pU pMb evalQ
    , test_script_properties_for pU pU pU pU pQ pB pB pB pMb evalQ
    , test_script_properties_for pU pU pU pU pQ pI pI pI pMb evalQ
    , test_script_properties_for pU pU pU pU pQ pI pI pB pMb evalQ
    , test_script_properties_for pU pU pU pU pQ pI pB pI pMb evalQ
    , test_script_properties_for pU pU pU pU pQ pB pI pI pMb evalQ
    , test_script_properties_for pU pU pU pU pQ pS pS pS pMb evalQ
    , test_script_properties_for pU pU pU pU pQ pS pS pI pMb evalQ
    , test_script_properties_for pU pU pU pU pQ pS pI pS pMb evalQ
    , test_script_properties_for pU pU pU pU pQ pI pS pS pMb evalQ

    , test_script_properties_for pS pS pS pS pQ pU pU pU pMb evalQ
    , test_script_properties_for pS pS pS pS pQ pB pB pB pMb evalQ
    , test_script_properties_for pS pS pS pS pQ pI pI pI pMb evalQ
    , test_script_properties_for pS pS pS pS pQ pI pI pB pMb evalQ
    , test_script_properties_for pS pS pS pS pQ pI pB pI pMb evalQ
    , test_script_properties_for pS pS pS pS pQ pB pI pI pMb evalQ
    , test_script_properties_for pS pS pS pS pQ pS pS pS pMb evalQ
    , test_script_properties_for pS pS pS pS pQ pS pS pI pMb evalQ
    , test_script_properties_for pS pS pS pS pQ pS pI pS pMb evalQ
    , test_script_properties_for pS pS pS pS pQ pI pS pS pMb evalQ

    -- List
    , test_script_properties_for pU pU pU pU pQ pU pU pU pLs evalQ
    , test_script_properties_for pU pU pU pU pQ pB pB pB pLs evalQ
    , test_script_properties_for pU pU pU pU pQ pI pI pI pLs evalQ
    , test_script_properties_for pU pU pU pU pQ pI pI pB pLs evalQ
    , test_script_properties_for pU pU pU pU pQ pI pB pI pLs evalQ
    , test_script_properties_for pU pU pU pU pQ pB pI pI pLs evalQ
    , test_script_properties_for pU pU pU pU pQ pS pS pS pLs evalQ
    , test_script_properties_for pU pU pU pU pQ pS pS pI pLs evalQ
    , test_script_properties_for pU pU pU pU pQ pS pI pS pLs evalQ
    , test_script_properties_for pU pU pU pU pQ pI pS pS pLs evalQ

    , test_script_properties_for pS pS pS pS pQ pU pU pU pLs evalQ
    , test_script_properties_for pS pS pS pS pQ pB pB pB pLs evalQ
    , test_script_properties_for pS pS pS pS pQ pI pI pI pLs evalQ
    , test_script_properties_for pS pS pS pS pQ pI pI pB pLs evalQ
    , test_script_properties_for pS pS pS pS pQ pI pB pI pLs evalQ
    , test_script_properties_for pS pS pS pS pQ pB pI pI pLs evalQ
    , test_script_properties_for pS pS pS pS pQ pS pS pS pLs evalQ
    , test_script_properties_for pS pS pS pS pQ pS pS pI pLs evalQ
    , test_script_properties_for pS pS pS pS pQ pS pI pS pLs evalQ
    , test_script_properties_for pS pS pS pS pQ pI pS pS pLs evalQ

    -- Either
    , test_script_properties_for pU pU pU pU pQ pU pU pU pEi evalQ
    , test_script_properties_for pU pU pU pU pQ pB pB pB pEi evalQ
    , test_script_properties_for pU pU pU pU pQ pI pI pI pEi evalQ
    , test_script_properties_for pU pU pU pU pQ pI pI pB pEi evalQ
    , test_script_properties_for pU pU pU pU pQ pI pB pI pEi evalQ
    , test_script_properties_for pU pU pU pU pQ pB pI pI pEi evalQ
    , test_script_properties_for pU pU pU pU pQ pS pS pS pEi evalQ
    , test_script_properties_for pU pU pU pU pQ pS pS pI pEi evalQ
    , test_script_properties_for pU pU pU pU pQ pS pI pS pEi evalQ
    , test_script_properties_for pU pU pU pU pQ pI pS pS pEi evalQ

    , test_script_properties_for pS pS pS pS pQ pU pU pU pEi evalQ
    , test_script_properties_for pS pS pS pS pQ pB pB pB pEi evalQ
    , test_script_properties_for pS pS pS pS pQ pI pI pI pEi evalQ
    , test_script_properties_for pS pS pS pS pQ pI pI pB pEi evalQ
    , test_script_properties_for pS pS pS pS pQ pI pB pI pEi evalQ
    , test_script_properties_for pS pS pS pS pQ pB pI pI pEi evalQ
    , test_script_properties_for pS pS pS pS pQ pS pS pS pEi evalQ
    , test_script_properties_for pS pS pS pS pQ pS pS pI pEi evalQ
    , test_script_properties_for pS pS pS pS pQ pS pI pS pEi evalQ
    , test_script_properties_for pS pS pS pS pQ pI pS pS pEi evalQ
    ]



data Q a = Q a

evalQ :: Q a -> a
evalQ (Q a) = a

pQ = Proxy :: Proxy Q
pU = Proxy :: Proxy ()
pB = Proxy :: Proxy Bool
pI = Proxy :: Proxy Int
pS = Proxy :: Proxy String

pId = Proxy :: Proxy Identity
pMb = Proxy :: Proxy Maybe
pLs = Proxy :: Proxy []
pEi = Proxy :: Proxy (Either Int)
