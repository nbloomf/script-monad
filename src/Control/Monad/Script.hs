{- |
Module      : Control.Monad.Script
Description : An unrolled stack of Reader, Writer, Error, State, and Prompt.
Copyright   : 2018, Automattic, Inc.
License     : BSD3
Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
Stability   : experimental
Portability : POSIX

@Script@ is an unrolled stack of reader, writer, state, error, and prompt monads, meant as a basis for building more specific DSLs. The addition of prompt to the monad team makes it straightforward to build effectful computations which defer the actual effects (and effect types) to an evaluator function that is both precisely controlled and easily extended.

The name 'Script' is meant to evoke the script of a play. In the theater sense a script is not a list of /instructions/ so much as a list of /suggestions/, and every cast gives a unique interpretation. Similarly a 'Script' is a pure value that gets an effectful interpretation from a user-supplied evaluator.
-}

{-# LANGUAGE Rank2Types, TupleSections, ScopedTypeVariables #-}
module Control.Monad.Script (
  -- * Script
    Script()

  , execScriptC
  , execScript
  , execScriptM

  -- ** Error
  , except
  , triage
  , throw
  , catch

  -- ** Reader
  , ask
  , local
  , transport
  , reader

  -- ** Writer
  , tell
  , listen
  , pass
  , censor

  -- ** State
  , get
  , put
  , modify
  , modify'
  , gets

  -- ** Prompt
  , prompt
) where


import Control.Monad (ap)
import Data.Monoid
import Data.Typeable (Typeable)
import Test.QuickCheck (Gen, Arbitrary(..), CoArbitrary(..))



-- | Opaque stack of error, reader, writer, state, and prompt monads.
newtype Script e r w s p a = Script
  { runScript :: (s,r) -> forall v. ((Either e a, s, w) -> v) -> (forall u. p u -> (u -> v) -> v) -> v
  } deriving Typeable



-- | Execute a 'Script' with a specified initial state, environment, and continuation.
execScriptC
  :: s -- ^ Initial state
  -> r -- ^ Environment
  -> ((Either e a, s, w) -> v)
  -> (forall u. p u -> (u -> v) -> v)
  -> Script e r w s p a
  -> v
execScriptC s r end cont x =
  runScript x (s,r) end cont


-- | Execute a 'Script' with a specified initial state and environment, and with a pure evaluator.
execScript
  :: s -- ^ Initial state
  -> r -- ^ Environment
  -> (forall u. p u -> u) -- ^ Pure evaluator
  -> Script e r w s p t
  -> (Either e t, s, w)
execScript s r eval =
  execScriptC s r id (\p c -> c $ eval p)


-- | Execute a 'Script' with a specified inital state and environment, and with a monadic evaluator.
execScriptM
  :: (Monad m)
  => s -- ^ Initial state
  -> r -- ^ Environment
  -> (forall u. p u -> m u) -- ^ Monadic evaluator
  -> Script e r w s p t
  -> m (Either e t, s, w)
execScriptM s r eval =
  execScriptC s r return (\p c -> eval p >>= c)


instance (Monoid w) => Monad (Script e r w s p) where
  return x = Script $ \(s,_) -> \end _ ->
    end (Right x, s, mempty)

  x >>= f = Script $ \(s0,r) -> \end cont ->
    let
      g (z1,s1,w1) =
        let
          h (z2,s2,w2) = end (z2, s2, mappend w1 w2)
        in
          case z1 of
            Right y -> runScript (f y) (s1,r) h cont
            Left e -> end (Left e, s1, w1)
    in
      runScript x (s0,r) g cont


instance (Monoid w) => Applicative (Script e r w s p) where
  pure = return
  (<*>) = ap


instance (Monoid w) => Functor (Script e r w s p) where
  fmap f x = x >>= (return . f)



-- | Retrieve the environment.
ask
  :: (Monoid w)
  => Script e r w s p r
ask = Script $ \(s,r) -> \end _ ->
  end (Right r, s, mempty)



-- | Run an action with a locally adjusted environment of the same type.
local
  :: (r -> r)
  -> Script e r w s p a
  -> Script e r w s p a
local = transport



-- | Run an action with a locally adjusted environment of a possibly different type.
transport
  :: (r2 -> r1)
  -> Script e r1 w s p a
  -> Script e r2 w s p a
transport f x = Script $ \(s,r) -> \end cont ->
  runScript x (s, f r) end cont



-- | Retrieve the image of the environment under a given function.
reader
  :: (Monoid w)
  => (r -> a)
  -> Script e r w s p a
reader f = fmap f ask



-- | Retrieve the current state.
get
  :: (Monoid w)
  => Script e r w s p s
get = Script $ \(s,_) -> \end _ ->
  end (Right s, s, mempty)



-- | Set the state.
put
  :: (Monoid w)
  => s
  -> Script e r w s p ()
put s = Script $ \(_,_) -> \end _ ->
  end (Right (), s, mempty)



-- | Modify the current state lazily.
modify
  :: (Monoid w)
  => (s -> s)
  -> Script e r w s p ()
modify f = Script $ \(s,_) -> \end _ ->
  end (Right (), f s, mempty)



-- | Modify the current state strictly.
modify'
  :: (Monoid w)
  => (s -> s)
  -> Script e r w s p ()
modify' f = Script $ \(s,_) -> \end _ ->
  end (Right (), f $! s, mempty)



-- | Retrieve the image of the current state under a given function.
gets
  :: (Monoid w)
  => (s -> a)
  -> Script e r w s p a
gets f = Script $ \(s,_) -> \end _ ->
  end (Right (f s), s, mempty)



-- | Write to the log.
tell
  :: w
  -> Script e r w s p ()
tell w = Script $ \(s,_) -> \end _ ->
  end (Right (), s, w)



-- | Run an action and attach the log to the result.
listen
  :: Script e r w s p a
  -> Script e r w s p (a,w)
listen x = Script $ \(r,s) -> \end cont ->
  runScript x (r,s)
    (\(y,s,w) -> end (fmap (,w) y, s, w)) cont



-- | Run an action that returns a value and a log-adjusting function, and apply the function to the local log.
pass
  :: Script e r w s p (a, w -> w)
  -> Script e r w s p a
pass x = Script $ \(r,s) -> \end cont ->
  let
    end' (z,s1,w) = case z of
      Right (y,f) -> end (Right y, s1, f w)
      Left e -> end (Left e, s1, w)
  in
    runScript x (r,s) end' cont



-- | Run an action, applying a function to the local log.
censor
  :: (w -> w)
  -> Script e r w s p a
  -> Script e r w s p a
censor f x = pass $ Script $ \(s,r) -> \end cont ->
  let
    end' (z,s1,w) = case z of
      Right y -> end (Right (y,f), s1, w)
      Left e -> end (Left e, s1, w)
  in
    runScript x (s,r) end' cont



-- | Inject an 'Either' into a 'Script'.
except
  :: (Monoid w)
  => Either e a
  -> Script e r w s p a
except z = Script $ \(s,_) -> \end _ ->
  end (z, s, mempty)



-- | Run an action, applying a function to any error.
triage
  :: (Monoid w)
  => (e1 -> e2)
  -> Script e1 r w s p a
  -> Script e2 r w s p a
triage f x = Script $ \(s,r) -> \end cont ->
  let
    end' (z,s1,w) = case z of
      Right y -> end (Right y, s1, w)
      Left e -> end (Left (f e), s1, w)
  in
    runScript x (s,r) end' cont



-- | Construct an error.
throw
  :: (Monoid w)
  => e
  -> Script e r w s p a
throw e = Script $ \(s,_) -> \end _ ->
  end (Left e, s, mempty)



-- | Run an action, applying a handler in case of an error result.
catch
  :: Script e r w s p a
  -> (e -> Script e r w s p a)
  -> Script e r w s p a
catch x h = Script $ \(s,r) -> \end cont ->
  let
    end' (z,s1,w) = case z of
      Right y -> end (Right y, s1, w)
      Left e -> runScript (h e) (s1,r) end cont
  in
    runScript x (s,r) end' cont



-- | Inject an atomic effect.
prompt
  :: (Monoid w)
  => p a
  -> Script e r w s p a
prompt p = Script $ \(s,_) -> \end cont ->
  cont p (\a -> end (Right a, s, mempty))



instance (Monoid w, Arbitrary a, CoArbitrary a)
  => Arbitrary (Script e r w s p a) where
  arbitrary = do
    a <- arbitrary :: Gen a
    k <- arbitrary :: Gen Int
    if k`rem`2 == 0
      then return $ return a
      else do
        f <- arbitrary :: Gen (a -> Script e r w s p a)
        return $ return a >>= f

instance Show (Script e r w s p a) where
  show _ = "<Script>"
