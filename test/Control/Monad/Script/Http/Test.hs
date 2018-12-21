{-# LANGUAGE OverloadedStrings, RecordWildCards, Rank2Types, BangPatterns #-}
module Control.Monad.Script.Http.Test (
    tests
) where

import Control.Concurrent.MVar
import Control.Monad.Trans.Identity
import System.IO
import Data.Functor.Identity
  ( Identity(..) )
import Data.Proxy
import Data.String
import Data.ByteString.Lazy
  ( ByteString, pack )
import Data.Typeable
import Data.List (isSuffixOf)

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Control.Monad.Script.Http
import Data.MockIO

import Data.MockIO.FileSystem
import Data.MockIO.Test.Server


tests :: Int -> MVar () -> TestTree
tests num lock = testGroup "Http"
  [ localOption (QuickCheckTests $ 10 * num) $ testGroup "Mock IO"
    [ testGroup "Internal"
      [ testProperty "comment: return ()" $
        prop_comment_value pU pU pU pU (evalMockIO evalId) (toIO simpleMockWorld) undefined
      , testProperty "comment: state unchanged" $
        prop_comment_state pU pU pU pU (evalMockIO evalId) (toIO simpleMockWorld) undefined
      , testProperty "wait: return ()" $
        prop_wait_value pU pU pU pU (evalMockIO evalId) (toIO simpleMockWorld) undefined
      , testProperty "wait: state unchanged" $
        prop_wait_state pU pU pU pU (evalMockIO evalId) (toIO simpleMockWorld) undefined
      , testProperty "httpGet: return" $
        prop_httpGet pU pU pU pU (evalMockIO evalId) (toIO simpleMockWorld) undefined
      , testProperty "httpSilentGet: return" $
        prop_httpSilentGet pU pU pU pU (evalMockIO evalId) (toIO simpleMockWorld) undefined
      , testProperty "httpPost: return ()" $
        prop_httpPost pU pU pU pU (evalMockIO evalId) (toIO simpleMockWorld) undefined
      , testProperty "httpSilentPost: return ()" $
        prop_httpSilentPost pU pU pU pU (evalMockIO evalId) (toIO simpleMockWorld) undefined
      , testProperty "throwError: is caught" $
        prop_throwError pU pU pU pU (evalMockIO evalId) (toIO simpleMockWorld) undefined
      , testProperty "throwJsonError: is caught" $
        prop_throwJsonError pU pU pU pU (evalMockIO evalId) (toIO simpleMockWorld) undefined
      , testProperty "parseJson: valid" $
        prop_parseJson pU pU pU pU (evalMockIO evalId) (toIO simpleMockWorld) undefined
      , testProperty "lookupKeyJson: valid" $
        prop_lookupKeyJson pU pU pU pU (evalMockIO evalId) (toIO simpleMockWorld) undefined
      , testProperty "logEntries: value" $
        prop_logEntries_log pU pU pU pU (evalMockIO evalId) (toIO simpleMockWorld) undefined
      ]
    , testGroup "External"
      [ testProperty "comment: is logged" $
        prop_comment_write pU pU pU pU (evalMockIO evalId) (toIOs simpleMockWorld) undefined
      , testProperty "comment: silent" $
        prop_comment_silent pU pU pU pU (evalMockIO evalId) (toIOs simpleMockWorld) undefined
      , testProperty "wait: is logged" $
        prop_wait_write pU pU pU pU (evalMockIO evalId) (toIOs simpleMockWorld) undefined
      , testProperty "wait: silent" $
        prop_wait_silent pU pU pU pU (evalMockIO evalId) (toIOs simpleMockWorld) undefined
      , testProperty "httpGet: is logged" $
        prop_httpGet_write pU pU pU pU (evalMockIO evalId) (toIOs simpleMockWorld) undefined
      , testProperty "httpGet: json" $
        prop_httpGet_json pU pU pU pU (evalMockIO evalId) (toIOs simpleMockWorld) undefined
      , testProperty "httpGet: silent" $
        prop_httpGet_silent pU pU pU pU (evalMockIO evalId) (toIOs simpleMockWorld) undefined
      , testProperty "httpSilentGet: is not logged" $
        prop_httpSilentGet_write pU pU pU pU (evalMockIO evalId) (toIOs simpleMockWorld) undefined
      , testProperty "throwError: is logged" $
        prop_throwError_write pU pU pU pU (evalMockIO evalId) (toIOs simpleMockWorld) undefined
      , testProperty "logDebug: is logged" $
        prop_logDebug_write pU pU pU pU (evalMockIO evalId) (toIOs simpleMockWorld) undefined
      ]
    ]
  , localOption (QuickCheckTests num) $ testGroup "Real IO"
    [ testGroup "Internal"
      [ real_internal_tests_for pU pU pU pU
      , real_internal_tests_for pI pI pI pI
      ]
    ]
  ]

real_internal_tests_for
  :: ( Eq s, Eq e, Eq w
     , Show s, Show r, Show e, Show w
     , Arbitrary s, Arbitrary r, Arbitrary e, Arbitrary w
     , Typeable e, Typeable r, Typeable w, Typeable s
     )
  => Proxy e -> Proxy r -> Proxy w -> Proxy s -> TestTree
real_internal_tests_for pe pr pw ps =
  let
    label =
      "e: " ++ show (typeRep pe) ++ "; " ++
      "r: " ++ show (typeRep pr) ++ "; " ++
      "w: " ++ show (typeRep pw) ++ "; " ++
      "s: " ++ show (typeRep ps) ++ " "
  in
    testGroup label
      [ testProperty "comment: return ()" $
        prop_comment_value pe pr pw ps (evalIO evalId) runIdentityT undefined
      , testProperty "comment: state unchanged" $
        prop_comment_state pe pr pw ps (evalIO evalId) runIdentityT undefined
      , testProperty "wait: return ()" $
        prop_wait_value pe pr pw ps (evalIO evalId) runIdentityT undefined
      , testProperty "wait: state unchanged" $
        prop_wait_state pe pr pw ps (evalIO evalId) runIdentityT undefined
      , testProperty "httpGet: return" $
        prop_httpGet pe pr pw ps (evalIO evalId) runIdentityT undefined
      , testProperty "httpSilentGet: return" $
        prop_httpSilentGet pe pr pw ps (evalIO evalId) runIdentityT undefined
      , testProperty "httpPost: return ()" $
        prop_httpPost pe pr pw ps (evalIO evalId) runIdentityT undefined
      , testProperty "httpSilentPost: return ()" $
        prop_httpSilentPost pe pr pw ps (evalIO evalId) runIdentityT undefined
      , testProperty "throwError: is caught" $
        prop_throwError pe pr pw ps (evalIO evalId) runIdentityT undefined
      , testProperty "throwJsonError: is caught" $
        prop_throwJsonError pe pr pw ps (evalIO evalId) runIdentityT undefined
      , testProperty "parseJson: valid" $
        prop_parseJson pe pr pw ps (evalIO evalId) runIdentityT undefined
      , testProperty "lookupKeyJson: valid" $
        prop_lookupKeyJson pe pr pw ps (evalIO evalId) runIdentityT undefined
      , testProperty "logEntries: value" $
        prop_logEntries_log pe pr pw ps (evalIO evalId) runIdentityT undefined
      ]


pU :: Proxy ()
pU = Proxy

pI :: Proxy Int
pI = Proxy

toIO :: MockWorld s -> IdentityT (MockIO s) a -> IO a
toIO u (IdentityT (MockIO x)) = do
  let (a,_) = x u
  return a

toIOs :: MockWorld s -> IdentityT (MockIO s) a -> IO (a, MockWorld s)
toIOs u (IdentityT (MockIO x)) = do
  return $ x u

as :: a -> a -> a
as _ a = a

testEnv :: r -> R e w r
testEnv r = (trivialEnv r)
  { _logHandle = stdout
  , _logOptions = trivialLogOptions
    { _logSilent = True
    }
  }

jsonEnv :: r -> R e w r
jsonEnv r = (trivialEnv r)
  { _logHandle = stdout
  , _logOptions = trivialLogOptions
    { _logSilent = False
    , _logJson = True
    , _logColor = False
    }
  }

noisyEnv :: r -> R e w r
noisyEnv r = (trivialEnv r)
  { _logHandle = stdout
  , _logOptions = trivialLogOptions
    { _logSilent = False
    }
  }

prop_comment_value
  :: (Monad eff, Show e, Show w, Show s)
  => Proxy e -> Proxy r -> Proxy w -> Proxy s
  -> (forall u. P p u -> eff u)
  -> (forall e s w t. IdentityT eff (Either (E e) t, S s, W e w) -> IO (Either (E e) t, S s, W e w))
  -> HttpT e r w s p eff ()
  -> s -> r -> String -> Property
prop_comment_value _ _ _ _ eval cond x s r msg =
  checkHttpTT (basicState s) (testEnv r) eval cond
    (hasValue (== ())) $
    as x $ do
      comment msg

prop_comment_state
  :: (Monad eff, Eq s, Show e, Show w, Show s)
  => Proxy e -> Proxy r -> Proxy w -> Proxy s
  -> (forall u. P p u -> eff u)
  -> (forall e s w t. IdentityT eff (Either (E e) t, S s, W e w) -> IO (Either (E e) t, S s, W e w))
  -> HttpT e r w s p eff ()
  -> s -> r -> String -> Property
prop_comment_state _ _ _ _ eval cond x s r msg =
  checkHttpTT (basicState s) (testEnv r) eval cond
    (hasState (== s)) $
    as x $ do
      comment msg

prop_comment_write
  :: (Monad eff, Show e, Show w, Show s, Show u)
  => Proxy e -> Proxy r -> Proxy w -> Proxy s
  -> (forall u. P p u -> eff u)
  -> (forall e s w t. IdentityT eff (Either (E e) t, S s, W e w) -> IO ((Either (E e) t, S s, W e w), MockWorld u))
  -> HttpT e r w s p eff ()
  -> s -> r -> String -> Property
prop_comment_write _ _ _ _ eval cond x s r str =
  let msg = filter (/= '\n') str in
  checkHttpTT (basicState s) (noisyEnv r) eval cond
    (hasWorld $ outputContains msg) $
    as x $ do
      comment msg

prop_comment_silent
  :: (Monad eff, Show e, Show w, Show s, Show u)
  => Proxy e -> Proxy r -> Proxy w -> Proxy s
  -> (forall u. P p u -> eff u)
  -> (forall e s w t. IdentityT eff (Either (E e) t, S s, W e w) -> IO ((Either (E e) t, S s, W e w), MockWorld u))
  -> HttpT e r w s p eff ()
  -> s -> r -> String -> Property
prop_comment_silent _ _ _ _ eval cond x s r str =
  let msg = filter (/= '\n') str in
  checkHttpTT (basicState s) (testEnv r) eval cond
    (hasWorld outputIsEmpty) $
    as x $ do
      comment msg

hasWorld
  :: (MockWorld u -> Bool)
  -> (v, MockWorld u)
  -> Bool
hasWorld p (_,w) = p w

outputContains
  :: String
  -> MockWorld u
  -> Bool
outputContains str world =
  case getLines (Right stdout) $ _files world of
    Nothing -> False
    Just ls -> any (isSuffixOf str) ls

outputIsEmpty
  :: MockWorld u -> Bool
outputIsEmpty world =
  _files world == emptyFileSystem

prop_wait_value
  :: (Monad eff, Show e, Show w, Show s)
  => Proxy e -> Proxy r -> Proxy w -> Proxy s
  -> (forall u. P p u -> eff u)
  -> (forall e s w t. IdentityT eff (Either (E e) t, S s, W e w) -> IO (Either (E e) t, S s, W e w))
  -> HttpT e r w s p eff ()
  -> s -> r -> Int -> Property
prop_wait_value _ _ _ _ eval cond x s r k =
  checkHttpTT (basicState s) (testEnv r) eval cond
    (hasValue (== ())) $
    as x $ do
      wait k

prop_wait_state
  :: (Monad eff, Eq s, Show e, Show w, Show s)
  => Proxy e -> Proxy r -> Proxy w -> Proxy s
  -> (forall u. P p u -> eff u)
  -> (forall e s w t. IdentityT eff (Either (E e) t, S s, W e w) -> IO (Either (E e) t, S s, W e w))
  -> HttpT e r w s p eff ()
  -> s -> r -> Int -> Property
prop_wait_state _ _ _ _ eval cond x s r k =
  checkHttpTT (basicState s) (testEnv r) eval cond
    (hasState (== s)) $
    as x $ do
      wait k

prop_wait_write
  :: (Monad eff, Show e, Show w, Show s, Show u)
  => Proxy e -> Proxy r -> Proxy w -> Proxy s
  -> (forall u. P p u -> eff u)
  -> (forall e s w t. IdentityT eff (Either (E e) t, S s, W e w) -> IO ((Either (E e) t, S s, W e w), MockWorld u))
  -> HttpT e r w s p eff ()
  -> s -> r -> Int -> Property
prop_wait_write _ _ _ _ eval cond x s r k =
  checkHttpTT (basicState s) (noisyEnv r) eval cond
    (hasWorld $ outputContains $ "Wait for " ++ show k ++ "μs") $
    as x $ do
      wait k

prop_wait_silent
  :: (Monad eff, Show e, Show w, Show s, Show u)
  => Proxy e -> Proxy r -> Proxy w -> Proxy s
  -> (forall u. P p u -> eff u)
  -> (forall e s w t. IdentityT eff (Either (E e) t, S s, W e w) -> IO ((Either (E e) t, S s, W e w), MockWorld u))
  -> HttpT e r w s p eff ()
  -> s -> r -> Int -> Property
prop_wait_silent _ _ _ _ eval cond x s r k =
  checkHttpTT (basicState s) (testEnv r) eval cond
    (hasWorld outputIsEmpty) $
    as x $ do
      wait k

prop_httpGet
  :: (Monad eff, Show e, Show w, Show s)
  => Proxy e -> Proxy r -> Proxy w -> Proxy s
  -> (forall u. P p u -> eff u)
  -> (forall e s w t. IdentityT eff (Either (E e) t, S s, W e w) -> IO (Either (E e) t, S s, W e w))
  -> HttpT e r w s p eff ()
  -> s -> r -> Property
prop_httpGet _ _ _ _ eval cond x s r =
  checkHttpTT (basicState s) (testEnv r) eval cond
    (hasValue (== ())) $
    as x $ do
      httpGet "http://example.com"
      return ()

prop_httpGet_json
  :: (Monad eff, Show e, Show w, Show s, Show u)
  => Proxy e -> Proxy r -> Proxy w -> Proxy s
  -> (forall u. P p u -> eff u)
  -> (forall e s w t. IdentityT eff (Either (E e) t, S s, W e w) -> IO ((Either (E e) t, S s, W e w), MockWorld u))
  -> HttpT e r w s p eff ()
  -> s -> r -> Property
prop_httpGet_json _ _ _ _ eval cond x s r =
  checkHttpTT (basicState s) (jsonEnv r) eval cond
    (hasWorld $ outputContains "True") $
    as x $ do
      val1 <- httpGet "http://example.com/json"
        >>= (return . _responseBody)
        >>= parseJson
      val2 <- parseJson "{\"key\":\"value\"}"
      comment $ show $ val1 == val2
      return ()

prop_httpGet_write
  :: (Monad eff, Show e, Show w, Show s, Show u)
  => Proxy e -> Proxy r -> Proxy w -> Proxy s
  -> (forall u. P p u -> eff u)
  -> (forall e s w t. IdentityT eff (Either (E e) t, S s, W e w) -> IO ((Either (E e) t, S s, W e w), MockWorld u))
  -> HttpT e r w s p eff ()
  -> s -> r -> Property
prop_httpGet_write _ _ _ _ eval cond x s r =
  checkHttpTT (basicState s) (noisyEnv r) eval cond
    (hasWorld $ outputContains "GET http://example.com") $
    as x $ do
      httpGet "http://example.com"
      return ()

prop_httpGet_silent
  :: (Monad eff, Show e, Show w, Show s, Show u)
  => Proxy e -> Proxy r -> Proxy w -> Proxy s
  -> (forall u. P p u -> eff u)
  -> (forall e s w t. IdentityT eff (Either (E e) t, S s, W e w) -> IO ((Either (E e) t, S s, W e w), MockWorld u))
  -> HttpT e r w s p eff ()
  -> s -> r -> Property
prop_httpGet_silent _ _ _ _ eval cond x s r =
  checkHttpTT (basicState s) (testEnv r) eval cond
    (hasWorld outputIsEmpty) $
    as x $ do
      httpGet "http://example.com"
      return ()

prop_httpSilentGet
  :: (Monad eff, Show e, Show w, Show s)
  => Proxy e -> Proxy r -> Proxy w -> Proxy s
  -> (forall u. P p u -> eff u)
  -> (forall e s w t. IdentityT eff (Either (E e) t, S s, W e w) -> IO (Either (E e) t, S s, W e w))
  -> HttpT e r w s p eff ()
  -> s -> r -> Property
prop_httpSilentGet _ _ _ _ eval cond x s r =
  checkHttpTT (basicState s) (testEnv r) eval cond
    (hasValue (== ())) $
    as x $ do
      httpSilentGet "http://example.com"
      return ()

prop_httpSilentGet_write
  :: (Monad eff, Show e, Show w, Show s, Show u)
  => Proxy e -> Proxy r -> Proxy w -> Proxy s
  -> (forall u. P p u -> eff u)
  -> (forall e s w t. IdentityT eff (Either (E e) t, S s, W e w) -> IO ((Either (E e) t, S s, W e w), MockWorld u))
  -> HttpT e r w s p eff ()
  -> s -> r -> Property
prop_httpSilentGet_write _ _ _ _ eval cond x s r =
  checkHttpTT (basicState s) (noisyEnv r) eval cond
    (hasWorld $ not . outputContains "http://example.com") $
    as x $ do
      httpSilentGet "http://example.com"
      return ()

prop_httpPost
  :: (Monad eff, Show e, Show w, Show s)
  => Proxy e -> Proxy r -> Proxy w -> Proxy s
  -> (forall u. P p u -> eff u)
  -> (forall e s w t. IdentityT eff (Either (E e) t, S s, W e w) -> IO (Either (E e) t, S s, W e w))
  -> HttpT e r w s p eff ()
  -> s -> r -> String -> Property
prop_httpPost _ _ _ _ eval cond x s r payload =
  checkHttpTT (basicState s) (testEnv r) eval cond
    (hasValue (== ())) $
    as x $ do
      httpPost "http://example.com" (fromString payload)
      return ()

prop_httpSilentPost
  :: (Monad eff, Show e, Show w, Show s)
  => Proxy e -> Proxy r -> Proxy w -> Proxy s
  -> (forall u. P p u -> eff u)
  -> (forall e s w t. IdentityT eff (Either (E e) t, S s, W e w) -> IO (Either (E e) t, S s, W e w))
  -> HttpT e r w s p eff ()
  -> s -> r -> String -> Property
prop_httpSilentPost _ _ _ _ eval cond x s r payload =
  checkHttpTT (basicState s) (testEnv r) eval cond
    (hasValue (== ())) $
    as x $ do
      httpSilentPost "http://example.com" (fromString payload)
      return ()

prop_throwError
  :: (Monad eff, Eq e, Show e, Show w, Show s)
  => Proxy e -> Proxy r -> Proxy w -> Proxy s
  -> (forall u. P p u -> eff u)
  -> (forall e s w t. IdentityT eff (Either (E e) t, S s, W e w) -> IO (Either (E e) t, S s, W e w))
  -> HttpT e r w s p eff Int
  -> s -> r -> Int -> e -> Property
prop_throwError _ _ _ _ eval cond x s r k err =
  checkHttpTT (basicState s) (testEnv r) eval cond
    (hasValue (== k)) $
    as x $ do
      catchError (throwError err)
        (\e -> if e == err then return k else return (k+1))

prop_logDebug_write
  :: (Monad eff, Show e, Show w, Show s, Show u)
  => Proxy e -> Proxy r -> Proxy w -> Proxy s
  -> (forall u. P p u -> eff u)
  -> (forall e s w t. IdentityT eff (Either (E e) t, S s, W e w) -> IO ((Either (E e) t, S s, W e w), MockWorld u))
  -> HttpT e r w s p eff ()
  -> s -> r -> w -> Property
prop_logDebug_write _ _ _ _ eval cond x s r w =
  checkHttpTT (basicState s) (noisyEnv r) eval cond
    (hasWorld $ outputContains "LOG") $
    as x $ do
      logDebug w
      return ()

prop_logEntries_log
  :: (Monad eff, Eq w, Show e, Show w, Show s)
  => Proxy e -> Proxy r -> Proxy w -> Proxy s
  -> (forall u. P p u -> eff u)
  -> (forall e s w t. IdentityT eff (Either (E e) t, S s, W e w) -> IO (Either (E e) t, S s, W e w))
  -> HttpT e r w s p eff ()
  -> s -> r -> w -> w -> w -> Property
prop_logEntries_log _ _ _ _ eval cond x s r w1 w2 w3 =
  checkHttpTT (basicState s) (testEnv r) eval cond
    (hasLog $ \w -> [w1,w2,w3] == logEntries w) $
    as x $ do
      logDebug w1
      comment "hey!"
      logDebug w2
      logDebug w3
      return ()

prop_throwError_write
  :: (Monad eff, Show e, Show w, Show s, Show u)
  => Proxy e -> Proxy r -> Proxy w -> Proxy s
  -> (forall u. P p u -> eff u)
  -> (forall e s w t. IdentityT eff (Either (E e) t, S s, W e w) -> IO ((Either (E e) t, S s, W e w), MockWorld u))
  -> HttpT e r w s p eff ()
  -> s -> r -> e -> Property
prop_throwError_write _ _ _ _ eval cond x s r e =
  checkHttpTT (basicState s) (noisyEnv r) eval cond
    (hasWorld $ outputContains "ERROR") $
    as x $ do
      throwError e
      return ()

prop_throwJsonError
  :: (Monad eff, Show e, Show w, Show s)
  => Proxy e -> Proxy r -> Proxy w -> Proxy s
  -> (forall u. P p u -> eff u)
  -> (forall e s w t. IdentityT eff (Either (E e) t, S s, W e w) -> IO (Either (E e) t, S s, W e w))
  -> HttpT e r w s p eff Int
  -> s -> r -> Int -> JsonError -> Property
prop_throwJsonError _ _ _ _ eval cond x s r k err =
  checkHttpTT (basicState s) (testEnv r) eval cond
    (hasValue (== k)) $
    as x $ do
      catchJsonError (throwJsonError err)
        (\e -> if e == err then return k else return (k+1))

prop_parseJson
  :: (Monad eff, Show e, Show w, Show s)
  => Proxy e -> Proxy r -> Proxy w -> Proxy s
  -> (forall u. P p u -> eff u)
  -> (forall e s w t. IdentityT eff (Either (E e) t, S s, W e w) -> IO (Either (E e) t, S s, W e w))
  -> HttpT e r w s p eff ()
  -> s -> r -> Int -> Property
prop_parseJson _ _ _ _ eval cond x s r k =
  checkHttpTT (basicState s) (testEnv r) eval cond
    (hasValue (== ())) $
    as x $ do
      parseJson $ fromString $ "{ \"key\":" ++ show k ++ " }"
      return ()

prop_lookupKeyJson
  :: (Monad eff, Show e, Show w, Show s)
  => Proxy e -> Proxy r -> Proxy w -> Proxy s
  -> (forall u. P p u -> eff u)
  -> (forall e s w t. IdentityT eff (Either (E e) t, S s, W e w) -> IO (Either (E e) t, S s, W e w))
  -> HttpT e r w s p eff Int
  -> s -> r -> Int -> Property
prop_lookupKeyJson _ _ _ _ eval cond x s r k =
  checkHttpTT (basicState s) (testEnv r) eval cond
    (hasValue (== k)) $
    as x $ do
      obj <- parseJson $ fromString $ "{ \"key\":" ++ show k ++ " }"
      lookupKeyJson "key" obj >>= constructFromJson

hasValue
  :: (t -> Bool)
  -> (Either (E e) t, S s, W e w)
  -> Bool
hasValue p (x,_,_) = case x of
  Left e -> False
  Right a -> p a

hasLog
  :: (W e w -> Bool)
  -> (Either (E e) t, S s, W e w)
  -> Bool
hasLog p (_,_,w) = p w

hasState
  :: (s -> Bool)
  -> (Either (E e) t, S s, W e w)
  -> Bool
hasState p (_,s,_) = p $ _userState s

data Id a = Id { unId :: a }

evalId :: (Monad m) => Id a -> m a
evalId (Id a) = return a
