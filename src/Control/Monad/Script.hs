{- |
Module      : Control.Monad.Script
Description : An unrolled stack of Reader, Writer, Error, State, and Prompt.
Copyright   : 2018, Automattic, Inc.
License     : BSD3
Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
Stability   : experimental
Portability : POSIX

`Script` is an unrolled stack of reader, writer, state, error, and prompt monads, meant as a basis for building more specific DSLs. Also comes in monad transformer flavor with `ScriptT`.

The addition of prompt to the monad team makes it straightforward to build effectful computations which defer the actual effects (and effect types) to an evaluator function that is both precisely controlled and easily extended. This allows us to build testable and composable API layers.

The name 'Script' is meant to evoke the script of a play. In the theater sense a script is not a list of /instructions/ so much as a list of /suggestions/, and every cast gives a unique interpretation. Similarly a 'Script' is a pure value that gets an effectful interpretation from a user-supplied evaluator.
-}

{-# LANGUAGE Rank2Types, TupleSections, ScopedTypeVariables #-}
module Control.Monad.Script (
  -- * Script
    Script
  , execScript
  , execScriptM

  -- * ScriptT
  , ScriptT()
  , execScriptT
  , execScriptTM
  , lift

  -- * Error
  , except
  , triage
  , throw
  , catch

  -- * Reader
  , ask
  , local
  , transport
  , reader

  -- * Writer
  , tell
  , listen
  , pass
  , censor

  -- * State
  , get
  , put
  , modify
  , modify'
  , gets

  -- * Prompt
  , prompt

  -- * Testing
  , checkScript
  , checkScriptM
  , checkScriptT
  , checkScriptTM
) where



import Control.Monad
  ( ap, join )
import Data.Functor.Classes
  ()
import Data.Functor.Identity
  ( Identity(..) )
import Data.Monoid
  ()
import Data.Typeable
  ( Typeable )
import Test.QuickCheck
  ( Property, Gen, Arbitrary(..), CoArbitrary(..) )
import Test.QuickCheck.Monadic
  ( monadicIO, run, assert )





-- | Opaque transformer stack of error (@e@), reader (@r@), writer (@w@), state (@s@), and prompt (@p@) monads.
newtype ScriptT e r w s p m a = ScriptT
  { runScriptT
      :: (s,r)
      -> forall v.
           ((Either e a, s, w) -> m v)
        -> (forall u. p u -> (u -> m v) -> m v)
        -> m v
  } deriving Typeable

instance (Monoid w) => Monad (ScriptT e r w s p m) where
  return x = ScriptT $ \(s,_) -> \end _ -> end (Right x, s, mempty)

  x >>= f = ScriptT $ \(s0,r) -> \end cont -> do
    let
      g (z1,s1,w1) = case z1 of
        Right y -> do
          let h (z2,s2,w2) = end (z2, s2, mappend w1 w2)
          runScriptT (f y) (s1,r) h cont
        Left e -> end (Left e, s1, w1)
    runScriptT x (s0,r) g cont

instance (Monoid w) => Applicative (ScriptT e r w s p m) where
  pure = return
  (<*>) = ap

instance (Monoid w) => Functor (ScriptT e r w s p m) where
  fmap f x = x >>= (return . f)





-- | Opaque stack of error (@e@), reader (@r@), writer (@w@), state (@s@), and prompt (@p@) monads.
type Script e r w s p = ScriptT e r w s p Identity





-- | Execute a 'ScriptT' with a specified initial state, environment, and continuation.
execScriptTC
  :: s -- ^ Initial state
  -> r -- ^ Environment
  -> ((Either e a, s, w) -> m v)
  -> (forall u. p u -> (u -> m v) -> m v)
  -> ScriptT e r w s p m a
  -> m v
execScriptTC s r end cont x =
  runScriptT x (s,r) end cont

-- | Execute a 'ScriptT' with a specified initial state and environment, and with a pure evaluator.
execScriptT
  :: (Monad m)
  => s -- ^ Initial state
  -> r -- ^ Environment
  -> (forall u. p u -> u) -- ^ Pure effect evaluator
  -> ScriptT e r w s p m t
  -> m (Either e t, s, w)
execScriptT s r eval =
  execScriptTC s r return (\p c -> c $ eval p)

-- | Turn a `ScriptT` with a pure evaluator into a `Property`; for testing with QuickCheck. Wraps `execScriptT`.
checkScriptT
  :: (Monad m)
  => s -- ^ Initial state
  -> r -- ^ Environment
  -> (forall u. p u -> u) -- ^ Pure effect evaluator
  -> (m (Either e t, s, w) -> IO q) -- ^ Condense to `IO`
  -> (q -> Bool) -- ^ Result check
  -> ScriptT e r w s p m t
  -> Property
checkScriptT s r eval cond check script = monadicIO $ do
  let result = execScriptT s r eval script
  q <- run $ cond result
  assert $ check q

-- | Execute a 'ScriptT' with a specified inital state and environment, and with a monadic evaluator. In this case the inner monad @m@ will typically be a monad transformer over the effect monad @n@.
execScriptTM
  :: (Monad (m eff), Monad eff)
  => s -- ^ Initial state
  -> r -- ^ Environment
  -> (forall u. p u -> eff u) -- ^ Monadic effect evaluator
  -> (forall u. n u -> m eff u) -- ^ Lift effects to the inner monad
  -> ScriptT e r w s p (m eff) t
  -> m eff (Either e t, s, w)
execScriptTM s r eval lift =
  execScriptTC s r return
    (\p c -> (lift $ eval p) >>= c)

-- | Turn a `ScriptT` with a monadic evaluator into a `Property`; for testing with QuickCheck. Wraps `execScriptTM`.
checkScriptTM
  :: (Monad (m eff), Monad eff)
  => s -- ^ Initial state
  -> r -- ^ Environment
  -> (forall u. p u -> eff u) -- ^ Moandic effect evaluator
  -> (forall u. eff u -> m eff u) -- ^ Lift effects to the inner monad
  -> (m eff (Either e t, s, w) -> IO q) -- ^ Condense to `IO`
  -> (q -> Bool) -- ^ Result check
  -> ScriptT e r w s p (m eff) t
  -> Property
checkScriptTM s r eval lift cond check script = monadicIO $ do
  let result = execScriptTM s r eval lift script
  q <- run $ cond result
  assert $ check q



-- | Execute a 'Script' with a specified initial state, environment, and continuation.
execScriptC
  :: s -- ^ Initial state
  -> r -- ^ Environment
  -> ((Either e a, s, w) -> v)
  -> (forall u. p u -> (u -> v) -> v)
  -> Script e r w s p a
  -> v
execScriptC s r end cont x =
  let cont' p c = Identity $ cont p (runIdentity . c) in
  runIdentity $ runScriptT x (s,r) (Identity . end) cont'

-- | Execute a 'Script' with a specified initial state and environment, and with a pure evaluator.
execScript
  :: s -- ^ Initial state
  -> r -- ^ Environment
  -> (forall u. p u -> u) -- ^ Pure evaluator
  -> Script e r w s p t
  -> (Either e t, s, w)
execScript s r eval =
  execScriptC s r id (\p c -> c $ eval p)

-- | Turn a `Script` with a pure evaluator into a `Bool`; for testing with QuickCheck. Wraps `execScript`.
checkScript
  :: s -- ^ Initial state
  -> r -- ^ Environment
  -> (forall u. p u -> u) -- ^ Pure evaluator
  -> ((Either e t, s, w) -> q) -- ^ Condense
  -> (q -> Bool) -- ^ Result check
  -> Script e r w s p t
  -> Bool
checkScript s r eval cond check script =
  check $ cond $ execScript s r eval script

-- | Execute a 'Script' with a specified inital state and environment, and with a monadic evaluator.
execScriptM
  :: (Monad eff)
  => s -- ^ Initial state
  -> r -- ^ Environment
  -> (forall u. p u -> eff u) -- ^ Monadic evaluator
  -> Script e r w s p t
  -> eff (Either e t, s, w)
execScriptM s r eval =
  execScriptC s r return
    (\p c -> (eval p) >>= c)

-- | Turn a `Script` with a monadic evaluator into a `Property`; for testing with QuickCheck. Wraps `execScriptM`.
checkScriptM
  :: (Monad eff)
  => s -- ^ Initial state
  -> r -- ^ Environment
  -> (forall u. p u -> eff u) -- ^ Moandic effect evaluator
  -> (eff (Either e t, s, w) -> IO q) -- ^ Condense to `IO`
  -> (q -> Bool) -- ^ Result check
  -> Script e r w s p t
  -> Property
checkScriptM s r eval cond check script = monadicIO $ do
  let result = execScriptM s r eval script
  q <- run $ cond result
  assert $ check q



-- | Retrieve the environment.
ask
  :: (Monoid w)
  => ScriptT e r w s p m r
ask = ScriptT $ \(s,r) -> \end _ ->
  end (Right r, s, mempty)



-- | Run an action with a locally adjusted environment of the same type.
local
  :: (r -> r)
  -> ScriptT e r w s p m a
  -> ScriptT e r w s p m a
local = transport



-- | Run an action with a locally adjusted environment of a possibly different type.
transport
  :: (r2 -> r1)
  -> ScriptT e r1 w s p m a
  -> ScriptT e r2 w s p m a
transport f x = ScriptT $ \(s,r) -> \end cont ->
  runScriptT x (s, f r) end cont



-- | Retrieve the image of the environment under a given function.
reader
  :: (Monoid w)
  => (r -> a)
  -> ScriptT e r w s p m a
reader f = fmap f ask



-- | Retrieve the current state.
get
  :: (Monoid w)
  => ScriptT e r w s p m s
get = ScriptT $ \(s,_) -> \end _ ->
  end (Right s, s, mempty)



-- | Replace the state.
put
  :: (Monoid w)
  => s
  -> ScriptT e r w s p m ()
put s = ScriptT $ \(_,_) -> \end _ ->
  end (Right (), s, mempty)



-- | Modify the current state lazily.
modify
  :: (Monoid w)
  => (s -> s)
  -> ScriptT e r w s p m ()
modify f = ScriptT $ \(s,_) -> \end _ ->
  end (Right (), f s, mempty)



-- | Modify the current state strictly.
modify'
  :: (Monoid w)
  => (s -> s)
  -> ScriptT e r w s p m ()
modify' f = ScriptT $ \(s,_) -> \end _ ->
  end (Right (), f $! s, mempty)



-- | Retrieve the image of the current state under a given function.
gets
  :: (Monoid w)
  => (s -> a)
  -> ScriptT e r w s p m a
gets f = ScriptT $ \(s,_) -> \end _ ->
  end (Right (f s), s, mempty)



-- | Write to the log.
tell
  :: w
  -> ScriptT e r w s p m ()
tell w = ScriptT $ \(s,_) -> \end _ ->
  end (Right (), s, w)



-- | Run an action and attach the log to the result.
listen
  :: ScriptT e r w s p m a
  -> ScriptT e r w s p m (a,w)
listen x = ScriptT $ \(r,s) -> \end cont ->
  runScriptT x (r,s)
    (\(y,s,w) -> end (fmap (,w) y, s, w)) cont



-- | Run an action that returns a value and a log-adjusting function, and apply the function to the local log.
pass
  :: ScriptT e r w s p m (a, w -> w)
  -> ScriptT e r w s p m a
pass x = ScriptT $ \(r,s) -> \end cont ->
  let
    end' (z,s1,w) = case z of
      Right (y,f) -> end (Right y, s1, f w)
      Left e -> end (Left e, s1, w)
  in
    runScriptT x (r,s) end' cont



-- | Run an action, applying a function to the local log.
censor
  :: (w -> w)
  -> ScriptT e r w s p m a
  -> ScriptT e r w s p m a
censor f x = pass $ ScriptT $ \(s,r) -> \end cont ->
  let
    end' (z,s1,w) = case z of
      Right y -> end (Right (y,f), s1, w)
      Left e -> end (Left e, s1, w)
  in
    runScriptT x (s,r) end' cont



-- | Inject an 'Either' into a 'Script'.
except
  :: (Monoid w)
  => Either e a
  -> ScriptT e r w s p m a
except z = ScriptT $ \(s,_) -> \end _ ->
  end (z, s, mempty)



-- | Run an action, applying a function to any error.
triage
  :: (Monoid w)
  => (e1 -> e2)
  -> ScriptT e1 r w s p m a
  -> ScriptT e2 r w s p m a
triage f x = ScriptT $ \(s,r) -> \end cont ->
  let
    end' (z,s1,w) = case z of
      Right y -> end (Right y, s1, w)
      Left e -> end (Left (f e), s1, w)
  in
    runScriptT x (s,r) end' cont



-- | Raise an error.
throw
  :: (Monoid w)
  => e
  -> ScriptT e r w s p m a
throw e = ScriptT $ \(s,_) -> \end _ ->
  end (Left e, s, mempty)



-- | Run an action, applying a handler in case of an error result.
catch
  :: ScriptT e r w s p m a
  -> (e -> ScriptT e r w s p m a)
  -> ScriptT e r w s p m a
catch x h = ScriptT $ \(s,r) -> \end cont ->
  let
    end' (z,s1,w) = case z of
      Right y -> end (Right y, s1, w)
      Left e -> runScriptT (h e) (s1,r) end cont
  in
    runScriptT x (s,r) end' cont



-- | Inject an atomic effect.
prompt
  :: (Monoid w)
  => p a
  -> ScriptT e r w s p m a
prompt p = ScriptT $ \(s,_) -> \end cont ->
  cont p (\a -> end (Right a, s, mempty))



-- | Lift a computation in the base monad.
lift
  :: (Monoid w, Monad m)
  => m a
  -> ScriptT e r w s p m a
lift x = ScriptT $ \(s,_) -> \end _ ->
  x >>= \a -> end (Right a, s, mempty)



instance (Monad m, Monoid w, Arbitrary a, CoArbitrary a)
  => Arbitrary (ScriptT e r w s p m a) where
  arbitrary = do
    (a,b) <- arbitrary :: Gen (a,a)
    k <- arbitrary :: Gen Int
    if k`rem`2 == 0
      then return $ return a
      else do
        f <- arbitrary :: Gen (a -> ScriptT e r w s p m a)
        return $ f a >> lift (return b)

instance Show (ScriptT e r w s p m a) where
  show _ = "<Script>"
