{- |
Module      : Control.Monad.Script
Description : An unrolled stack of Reader, Writer, Error, State, and Prompt transformers.
Copyright   : 2018, Automattic, Inc.
License     : BSD3
Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
Stability   : experimental
Portability : POSIX

`ScriptT` is an unrolled stack of reader, writer, state, error, and prompt monad transformers, meant as a basis for building more specific DSLs. Also comes in "monad transformer transformer" flavor with `ScriptTT`.

The addition of prompt to the monad team makes it straightforward to build effectful computations which defer the actual effects (and effect types) to an evaluator function that is both precisely controlled and easily extended. This allows us to build testable and composable API layers.

The name "script" is meant to evoke the script of a play. In the theater sense a script is not a list of /instructions/ so much as a list of /suggestions/, and every cast gives a unique interpretation. Similarly a 'ScriptT eff a' is a pure value that gets an effectful interpretation in monad `eff` from a user-supplied evaluator.
-}

{-#
  LANGUAGE
    GADTs,
    Rank2Types,
    TupleSections, 
    KindSignatures,
    ScopedTypeVariables,
    QuantifiedConstraints
#-}

module Control.Monad.Script (
  -- * ScriptT
    ScriptT

  -- * ScriptTT
  , ScriptTT()
  , execScriptTT

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
  , draft
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
  , checkScriptTT
) where



import Control.Monad
  ( ap, join )
import Control.Monad.Trans.Class
  ( MonadTrans(..) )
import Control.Monad.Trans.Identity
  ( IdentityT(..) )
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





-- | Opaque stack of error (@e@), reader (@r@), writer (@w@), state (@s@), and prompt (@p@) monad transformers, accepting a monad transformer parameter (@t@). Behaves something like a monad transformer transformer.
data
  ScriptTT
    (e :: *)
    (r :: *)
    (w :: *)
    (s :: *)
    (p :: * -> *)
    (t :: (* -> *) -> * -> *)
    (eff :: * -> *)
    (a :: *)
  where
  ScriptTT
    :: (Monad eff, Monad (t eff), MonadTrans t)
    => ((s,r)
         -> forall v.
             ((Either e a, s, w) -> t eff v)
             -> (forall u. p u -> (u -> t eff v) -> t eff v)
             -> t eff v)
    -> ScriptTT e r w s p t eff a
  deriving Typeable

-- Only needed to make type inference work correctly.
runScriptTT
  :: ScriptTT e r w s p t eff a
  -> (s,r)
  -> forall v.
     ((Either e a, s, w) -> t eff v)
       -> (forall u. p u -> (u -> t eff v) -> t eff v)
       -> t eff v
runScriptTT (ScriptTT x) = x

instance (Monoid w, Monad eff, Monad (t eff), MonadTrans t)
  => Monad (ScriptTT e r w s p t eff) where
  return x = ScriptTT $ \(s,_) -> \end _ ->
    end (Right x, s, mempty)

  x >>= f = ScriptTT $ \(s0,r) -> \end cont -> do
    let
      g (z1,s1,w1) = case z1 of
        Right y -> do
          let h (z2,s2,w2) = end (z2, s2, mappend w1 w2)
          runScriptTT (f y) (s1,r) h cont
        Left e -> do
          let h (_,s2,w2) = end (Left e, s2, mappend w1 w2)
          runScriptTT (return ()) (s1,r) h cont
          
    runScriptTT x (s0,r) g cont

instance (Monoid w, Monad eff, Monad (t eff), MonadTrans t)
  => Applicative (ScriptTT e r w s p t eff) where
  pure = return
  (<*>) = ap

instance (Monoid w, Monad eff, Monad (t eff), MonadTrans t)
  => Functor (ScriptTT e r w s p t eff) where
  fmap f x = x >>= (return . f)

instance (Monoid w, forall m. (Monad m) => Monad (t m), MonadTrans t)
  => MonadTrans (ScriptTT e r w s p t) where
  lift x = ScriptTT $ \(s,_) -> \end _ ->
    lift x >>= \a -> end (Right a, s, mempty)





-- | Opaque stack of error (@e@), reader (@r@), writer (@w@), state (@s@), and prompt (@p@) monad transformers.
type ScriptT e r w s p = ScriptTT e r w s p IdentityT





-- Execute a `ScriptTT` with a specified initial state, environment, and continuation.
execScriptTC
  :: s -- ^ Initial state
  -> r -- ^ Environment
  -> ((Either e a, s, w) -> t eff v)
  -> (forall u. p u -> (u -> t eff v) -> t eff v)
  -> ScriptTT e r w s p t eff a
  -> t eff v
execScriptTC s r end cont (ScriptTT run) =
  run (s,r) end cont

-- | Execute a `ScriptTT` with a specified inital state and environment and with a specified prompt evaluator into the effect monad @eff@.
execScriptTT
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => s -- ^ Initial state
  -> r -- ^ Environment
  -> (forall u. p u -> eff u) -- ^ Monadic effect evaluator
  -> ScriptTT e r w s p t eff a
  -> t eff (Either e a, s, w)
execScriptTT s r eval =
  execScriptTC s r return
    (\p c -> (lift $ eval p) >>= c)

-- | Turn a `ScriptTT` with a monadic evaluator into a `Property`; for testing with QuickCheck. Wraps `execScriptTT`.
checkScriptTT
  :: (Monad eff, Monad (t eff), MonadTrans t, Show q)
  => s -- ^ Initial state
  -> r -- ^ Environment
  -> (forall u. p u -> eff u) -- ^ Moandic effect evaluator
  -> (t eff (Either e a, s, w) -> IO q) -- ^ Condense to `IO`
  -> (q -> Bool) -- ^ Result check
  -> ScriptTT e r w s p t eff a
  -> Property
checkScriptTT s r eval cond check script = monadicIO $ do
  let result = execScriptTT s r eval script
  q <- run $ cond result
  assert $ check q



-- | Retrieve the environment.
ask
  :: (Monoid w, Monad eff, Monad (t eff), MonadTrans t)
  => ScriptTT e r w s p t eff r
ask = ScriptTT $ \(s,r) -> \end _ ->
  end (Right r, s, mempty)



-- | Run an action with a locally adjusted environment of the same type.
local
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => (r -> r)
  -> ScriptTT e r w s p t eff a
  -> ScriptTT e r w s p t eff a
local = transport



-- | Run an action with a locally adjusted environment of a possibly different type.
transport
  :: (Monad eff, Monad (t eff), MonadTrans t)
  => (r2 -> r1)
  -> ScriptTT e r1 w s p t eff a
  -> ScriptTT e r2 w s p t eff a
transport f x = ScriptTT $ \(s,r) -> \end cont ->
  runScriptTT x (s, f r) end cont



-- | Retrieve the image of the environment under a given function.
reader
  :: (Monoid w, Monad eff, Monad (t eff), MonadTrans t, Monad (t eff))
  => (r -> a)
  -> ScriptTT e r w s p t eff a
reader f = fmap f ask



-- | Retrieve the current state.
get
  :: (Monoid w, Monad eff, Monad (t eff), MonadTrans t)
  => ScriptTT e r w s p t eff s
get = ScriptTT $ \(s,_) -> \end _ ->
  end (Right s, s, mempty)



-- | Replace the state.
put
  :: (Monoid w, Monad eff, Monad (t eff), MonadTrans t)
  => s
  -> ScriptTT e r w s p t eff ()
put s = ScriptTT $ \(_,_) -> \end _ ->
  end (Right (), s, mempty)



-- | Modify the current state lazily.
modify
  :: (Monoid w, Monad eff, Monad (t eff), MonadTrans t)
  => (s -> s)
  -> ScriptTT e r w s p t eff ()
modify f = ScriptTT $ \(s,_) -> \end _ ->
  end (Right (), f s, mempty)



-- | Modify the current state strictly.
modify'
  :: (Monoid w, Monad eff, Monad (t eff), MonadTrans t)
  => (s -> s)
  -> ScriptTT e r w s p t eff ()
modify' f = ScriptTT $ \(s,_) -> \end _ ->
  end (Right (), f $! s, mempty)



-- | Retrieve the image of the current state under a given function.
gets
  :: (Monoid w, Monad eff, Monad (t eff), MonadTrans t)
  => (s -> a)
  -> ScriptTT e r w s p t eff a
gets f = ScriptTT $ \(s,_) -> \end _ ->
  end (Right (f s), s, mempty)



-- | Write to the log.
tell
  :: (Monoid w, Monad eff, Monad (t eff), MonadTrans t)
  => w
  -> ScriptTT e r w s p t eff ()
tell w = ScriptTT $ \(s,_) -> \end _ ->
  end (Right (), s, w)



-- | Run an action and attach the log to the result, setting the log to `mempty`.
draft
  :: (Monoid w, Monad eff, Monad (t eff), MonadTrans t)
  => ScriptTT e r w s p t eff a
  -> ScriptTT e r w s p t eff (a,w)
draft x = ScriptTT $ \(r,s) -> \end cont ->
  runScriptTT x (r,s)
    (\(y,s,w) -> end (fmap (,w) y, s, mempty)) cont



-- | Run an action and attach the log to the result.
listen
  :: (Monoid w, Monad eff, Monad (t eff), MonadTrans t)
  => ScriptTT e r w s p t eff a
  -> ScriptTT e r w s p t eff (a,w)
listen x = ScriptTT $ \(r,s) -> \end cont ->
  runScriptTT x (r,s)
    (\(y,s,w) -> end (fmap (,w) y, s, w)) cont



-- | Run an action that returns a value and a log-adjusting function, and apply the function to the local log.
pass
  :: (Monoid w, Monad eff, Monad (t eff), MonadTrans t)
  => ScriptTT e r w s p t eff (a, w -> w)
  -> ScriptTT e r w s p t eff a
pass x = ScriptTT $ \(r,s) -> \end cont ->
  let
    end' (z,s1,w) = case z of
      Right (y,f) -> end (Right y, s1, f w)
      Left e -> end (Left e, s1, w)
  in
    runScriptTT x (r,s) end' cont



-- | Run an action, applying a function to the local log.
censor
  :: (Monoid w, Monad eff, Monad (t eff), MonadTrans t)
  => (w -> w)
  -> ScriptTT e r w s p t eff a
  -> ScriptTT e r w s p t eff a
censor f x = pass $ ScriptTT $ \(s,r) -> \end cont ->
  let
    end' (z,s1,w) = case z of
      Right y -> end (Right (y,f), s1, w)
      Left e -> end (Left e, s1, w)
  in
    runScriptTT x (s,r) end' cont



-- | Inject an 'Either' into a 'Script'.
except
  :: (Monoid w, Monad eff, Monad (t eff), MonadTrans t)
  => Either e a
  -> ScriptTT e r w s p t eff a
except z = ScriptTT $ \(s,_) -> \end _ ->
  end (z, s, mempty)



-- | Run an action, applying a function to any error.
triage
  :: (Monoid w, Monad eff, Monad (t eff), MonadTrans t)
  => (e1 -> e2)
  -> ScriptTT e1 r w s p t eff a
  -> ScriptTT e2 r w s p t eff a
triage f x = ScriptTT $ \(s,r) -> \end cont ->
  let
    end' (z,s1,w) = case z of
      Right y -> end (Right y, s1, w)
      Left e -> end (Left (f e), s1, w)
  in
    runScriptTT x (s,r) end' cont



-- | Raise an error.
throw
  :: (Monoid w, Monad eff, Monad (t eff), MonadTrans t)
  => e
  -> ScriptTT e r w s p t eff a
throw e = ScriptTT $ \(s,r) -> \end cont ->
  let end' (_,s1,w1) = end (Left e, s1, w1)
  in runScriptTT (return ()) (s,r) end' cont



-- | Run an action, applying a handler in case of an error result.
catch
  :: (Monoid w, Monad eff, Monad (t eff), MonadTrans t)
  => ScriptTT e r w s p t eff a
  -> (e -> ScriptTT e r w s p t eff a)
  -> ScriptTT e r w s p t eff a
catch (ScriptTT x) h = ScriptTT $ \(s,r) -> \end cont ->
  let
    end' (z,s1,w) = case z of
      Right y -> end (Right y, s1, w)
      Left e -> do
        let end'' (z2,s2,w2) = end (z2, s2, mappend w w2)
        runScriptTT (h e) (s1,r) end'' cont
  in
    x (s,r) end' cont



-- | Inject an atomic effect.
prompt
  :: (Monoid w, Monad eff, Monad (t eff), MonadTrans t)
  => p a
  -> ScriptTT e r w s p t eff a
prompt p = ScriptTT $ \(s,_) -> \end cont ->
  cont p (\a -> end (Right a, s, mempty))





instance
  ( Monoid w, Monad eff, forall m. Monad m => Monad (t m), MonadTrans t
  , Arbitrary a, CoArbitrary a
  ) => Arbitrary (ScriptTT e r w s p t eff a) where
  arbitrary = do
    (a,b) <- arbitrary :: Gen (a,a)
    k <- arbitrary :: Gen Int
    if k`rem`2 == 0
      then return $ return a
      else do
        f <- arbitrary :: Gen (a -> ScriptTT e r w s p t eff a)
        return $ f a >> lift (return b)

instance Show (ScriptTT e r w s p t eff a) where
  show _ = "<Script>"
