{-# LANGUAGE OverloadedStrings #-}
module Data.MockIO.Test (
    tests
) where

import Prelude hiding (lookup)
import Data.ByteString.Lazy (ByteString)
import Control.Exception (SomeException)
import System.IO (Handle, stdout, stderr)
import Data.Proxy
import Data.Typeable
import Text.Show.Functions

import Test.Tasty
import Test.Tasty.QuickCheck

import Data.MockIO.FileSystem
import Control.Monad.Script.Http
import Data.MockIO



tests :: Int -> TestTree
tests num = localOption (QuickCheckTests $ 50 * num) $
  testGroup "MockIO"
    [ test_mockio_properties
    , test_mocknet_properties
    ]



as :: x -> x -> x
as _ = id

pU = Proxy :: Proxy ()
pB = Proxy :: Proxy Bool
pI = Proxy :: Proxy Int




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

test_mockio_properties :: TestTree
test_mockio_properties =
  testGroup "MockIO Properties"
    [ test_mockio_properties_for pU pU pU pU
    , test_mockio_properties_for pB pB pB pB
    , test_mockio_properties_for pI pI pI pI
    ]

test_mockio_properties_for
  :: ( Eq a, Eq b, Eq c, Eq s
     , Show a, Show b, Show c, Show s
     , Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary s
     , CoArbitrary a, CoArbitrary b, CoArbitrary c, CoArbitrary s
     , Typeable a, Typeable b, Typeable c, Typeable s
     )
  => Proxy a -> Proxy b -> Proxy c -> Proxy s -> TestTree
test_mockio_properties_for pa pb pc ps =
  let
    label = "MockIO: " ++
      "s: " ++ show (typeRep ps) ++ " " ++
      "a: " ++ show (typeRep pa) ++ " " ++
      "b: " ++ show (typeRep pb) ++ " " ++
      "c: " ++ show (typeRep pc)
  in
    testGroup label
      [ testGroup "Monad Laws"
        [ testProperty "x >>= return === x" $
          prop_mockio_right_identity_law ps pa
        , testProperty "return a >>= f === f a" $
          prop_mockio_left_identity_law ps pa
        , testProperty "(x >>= f) >>= g === x >>= (\\z -> f z >>= g)" $
          prop_mockio_associativity_law pa pb pc ps
        ]
      , testGroup "Applicative Laws"
        [ testProperty "pure id <*> x === x" $
          prop_mockio_applicative_identity_law ps pa
        , testProperty "pure f <*> pure x === pure (f x)" $
          prop_mockio_applicative_hom_law ps pa pb
        , testProperty "u <*> pure x === pure ($ x) <*> u" $
          prop_mockio_applicative_interchange_law ps pa pb
        , testProperty "pure (.) <*> v <*> u <*> x === v <*> (u <*> x)" $
          prop_mockio_applicative_composition_law ps pa pb pc
        ]
      , testGroup "State Laws"
        [ testProperty "get >>= put === return ()" $
          prop_mockio_get_put_law ps
        , testProperty "put s >> put t === put t" $
          prop_mockio_put_put_law ps
        , testProperty "put s >> get === put s >> return s" $
          prop_mockio_put_get_law ps
        , testProperty "put s >> modify f === put (f s)" $
          prop_mockio_put_modify_law ps
        ]
      ]



prop_mockio_right_identity_law
  :: (Eq s, Eq a)
  => Proxy s -> Proxy a
  -> s -> MockIO s a -> Bool
prop_mockio_right_identity_law _ _ s x =
  mockIOEq s (x >>= return) x

prop_mockio_left_identity_law
  :: (Eq s, Eq a)
  => Proxy s -> Proxy a
  -> s -> a -> (a -> MockIO s a) -> Bool
prop_mockio_left_identity_law _ _ s a f =
  mockIOEq s (return a >>= f) (f a)

prop_mockio_associativity_law
  :: (Eq s, Eq c)
  => Proxy s -> Proxy a -> Proxy b -> Proxy c
  -> s -> MockIO s a
  -> (a -> MockIO s b)
  -> (b -> MockIO s c)
  -> Bool
prop_mockio_associativity_law _ _ _ _ s x f g =
  mockIOEq s ((x >>= f) >>= g) (x >>= (\z -> f z >>= g))

prop_mockio_get_put_law
  :: (Eq s)
  => Proxy s -> s -> MockIO s (MockWorld s) -> s -> Bool
prop_mockio_get_put_law _ u x s =
  mockIOEq u ((as x getMockWorld) >>= putMockWorld) (return ())

prop_mockio_put_put_law
  :: (Eq s)
  => Proxy s -> s -> MockIO s () -> MockWorld s -> MockWorld s -> Bool
prop_mockio_put_put_law _ u x s t =
  mockIOEq u
    ((as x $ putMockWorld s) >> (putMockWorld t))
    (putMockWorld t)

prop_mockio_put_get_law
  :: (Eq s)
  => Proxy s -> s -> MockWorld s -> MockIO s () -> Bool
prop_mockio_put_get_law _ s u x =
  mockIOEq s
    ((as x $ putMockWorld u) >> getMockWorld)
    ((as x $ putMockWorld u) >> return u)

prop_mockio_put_modify_law
  :: (Eq s)
  => Proxy s -> s -> MockWorld s
  -> (MockWorld s -> MockWorld s) -> MockIO s () -> Bool
prop_mockio_put_modify_law _ s u f x =
  mockIOEq s
    ((as x $ putMockWorld u) >> modifyMockWorld f)
    ((as x $ putMockWorld $ f u))

prop_mockio_applicative_identity_law
  :: (Eq a, Eq s)
  => Proxy s -> Proxy a
  -> s -> MockIO s a -> Bool
prop_mockio_applicative_identity_law _ _ s x =
  mockIOEq s (pure id <*> x) x

prop_mockio_applicative_hom_law
  :: (Eq b, Eq s)
  => Proxy s -> Proxy a -> Proxy b
  -> s -> (a -> b) -> a -> MockIO s a -> Bool
prop_mockio_applicative_hom_law _ _ _ s f a x =
  mockIOEq s
    (pure f <*> (as x $ pure a))
    (pure (f a))

prop_mockio_applicative_interchange_law
  :: (Eq a, Eq b, Eq s)
  => Proxy s -> Proxy a -> Proxy b
  -> s
  -> MockIO s (a -> b)
  -> a
  -> MockIO s a -> Bool
prop_mockio_applicative_interchange_law _ _ _ s u a x =
  mockIOEq s
    (u <*> (as x $ pure a))
    (pure ($ a) <*> u)

prop_mockio_applicative_composition_law
  :: (Eq c, Eq s)
  => Proxy s -> Proxy a -> Proxy b -> Proxy c
  -> s
  -> MockIO s (a -> b)
  -> MockIO s (b -> c)
  -> MockIO s a
  -> Bool
prop_mockio_applicative_composition_law _ _ _ _ s u v x =
  mockIOEq s
    (pure (.) <*> v <*> u <*> x)
    (v <*> (u <*> x))





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

test_mocknet_properties :: TestTree
test_mocknet_properties =
  testGroup "MockNetwork"
    [ test_mocknet_properties_for pU pU pU pU
    , test_mocknet_properties_for pB pB pB pB
    , test_mocknet_properties_for pI pI pI pI
    ]

test_mocknet_properties_for
  :: ( Eq a, Eq b, Eq c, Eq s
     , Show a, Show b, Show c, Show s
     , Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary s
     , CoArbitrary a, CoArbitrary b, CoArbitrary c, CoArbitrary s
     , Typeable a, Typeable b, Typeable c, Typeable s
     )
  => Proxy a -> Proxy b -> Proxy c -> Proxy s -> TestTree
test_mocknet_properties_for pa pb pc ps =
  let
    label = "MockIO: " ++
      "s: " ++ show (typeRep ps) ++ " " ++
      "a: " ++ show (typeRep pa) ++ " " ++
      "b: " ++ show (typeRep pb) ++ " " ++
      "c: " ++ show (typeRep pc)
  in
    testGroup label
      [ testGroup "Monad Properties"
        [ testProperty "x >>= return === x" $
          prop_mocknet_right_identity_law ps pa
        , testProperty "return a >>= f === f a" $
          prop_mocknet_left_identity_law ps pa
        , testProperty "(x >>= f) >>= g === x >>= (\\z -> f z >>= g)" $
          prop_mocknet_associativity_law pa pb pc ps
        ]
      , testGroup "Applicative Properties"
        [ testProperty "pure id <*> x === x" $
          prop_mocknet_applicative_identity_law ps pa
        , testProperty "pure f <*> pure x === pure (f x)" $
          prop_mocknet_applicative_hom_law ps pa pb
        , testProperty "u <*> pure x === pure ($ x) <*> u" $
          prop_mocknet_applicative_interchange_law ps pa pb
        , testProperty "pure (.) <*> v <*> u <*> x === v <*> (u <*> x)" $
          prop_mocknet_applicative_composition_law ps pa pb pc
        ]
      , testGroup "State Properties"
        [ testProperty "get >>= put === return ()" $
          prop_mocknet_get_put_law ps
        , testProperty "put s >> put t === put t" $
          prop_mocknet_put_put_law ps
        , testProperty "put s >> get === put s >> return s" $
          prop_mocknet_put_get_law ps
        , testProperty "put s >> modify f === put (f s)" $
          prop_mocknet_put_modify_law ps
        ]
      ]



prop_mocknet_right_identity_law
  :: (Eq s, Eq a)
  => Proxy s -> Proxy a
  -> s -> MockNetwork s a -> Bool
prop_mocknet_right_identity_law _ _ s x =
  mockNetEq s (x >>= return) x

prop_mocknet_left_identity_law
  :: (Eq s, Eq a)
  => Proxy s -> Proxy a
  -> s -> a -> (a -> MockNetwork s a) -> Bool
prop_mocknet_left_identity_law _ _ s a f =
  mockNetEq s (return a >>= f) (f a)

prop_mocknet_associativity_law
  :: (Eq s, Eq c)
  => Proxy s -> Proxy a -> Proxy b -> Proxy c
  -> s -> MockNetwork s a
  -> (a -> MockNetwork s b)
  -> (b -> MockNetwork s c)
  -> Bool
prop_mocknet_associativity_law _ _ _ _ s x f g =
  mockNetEq s ((x >>= f) >>= g) (x >>= (\z -> f z >>= g))

prop_mocknet_get_put_law
  :: (Eq s)
  => Proxy s -> s -> MockNetwork s s -> s -> Bool
prop_mocknet_get_put_law _ u x s =
  mockNetEq u ((as x getMockServer) >>= putMockServer) (return ())

prop_mocknet_put_put_law
  :: (Eq s)
  => Proxy s -> s -> MockNetwork s () -> s -> s -> Bool
prop_mocknet_put_put_law _ u x s t =
  mockNetEq u
    ((as x $ putMockServer s) >> (putMockServer t))
    (putMockServer t)

prop_mocknet_put_get_law
  :: (Eq s)
  => Proxy s -> s -> s -> MockNetwork s () -> Bool
prop_mocknet_put_get_law _ s u x =
  mockNetEq s
    ((as x $ putMockServer u) >> getMockServer)
    ((as x $ putMockServer u) >> return u)

prop_mocknet_put_modify_law
  :: (Eq s)
  => Proxy s -> s -> s -> (s -> s) -> MockNetwork s () -> Bool
prop_mocknet_put_modify_law _ s u f x =
  mockNetEq s
    ((as x $ putMockServer u) >> modifyMockServer f)
    ((as x $ putMockServer $ f u))

prop_mocknet_applicative_identity_law
  :: (Eq a, Eq s)
  => Proxy s -> Proxy a
  -> s -> MockNetwork s a -> Bool
prop_mocknet_applicative_identity_law _ _ s x =
  mockNetEq s (pure id <*> x) x

prop_mocknet_applicative_hom_law
  :: (Eq b, Eq s)
  => Proxy s -> Proxy a -> Proxy b
  -> s -> (a -> b) -> a -> MockNetwork s a -> Bool
prop_mocknet_applicative_hom_law _ _ _ s f a x =
  mockNetEq s
    (pure f <*> (as x $ pure a))
    (pure (f a))

prop_mocknet_applicative_interchange_law
  :: (Eq a, Eq b, Eq s)
  => Proxy s -> Proxy a -> Proxy b
  -> s
  -> MockNetwork s (a -> b)
  -> a
  -> MockNetwork s a -> Bool
prop_mocknet_applicative_interchange_law _ _ _ s u a x =
  mockNetEq s
    (u <*> (as x $ pure a))
    (pure ($ a) <*> u)

prop_mocknet_applicative_composition_law
  :: (Eq c, Eq s)
  => Proxy s -> Proxy a -> Proxy b -> Proxy c
  -> s
  -> MockNetwork s (a -> b)
  -> MockNetwork s (b -> c)
  -> MockNetwork s a
  -> Bool
prop_mocknet_applicative_composition_law _ _ _ _ s u v x =
  mockNetEq s
    (pure (.) <*> v <*> u <*> x)
    (v <*> (u <*> x))
