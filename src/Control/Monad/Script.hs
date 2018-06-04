{-# LANGUAGE Rank2Types, TupleSections #-}
module Control.Monad.Script (
  -- * Script
    Script()

  , execScriptC
  , execScript
  , execScriptM

  -- ** Reader
  , ask
  , local
  , transport
  , reader

  -- ** State
  , get
  , put
  , modify
  , modify'
  , gets

  -- ** Writer
  , tell
  , listen
  , pass
  , censor

  -- ** Error
  , except
  , triage
  , throw
  , catch

  -- ** Prompt
  , prompt
) where


import Control.Monad (ap)
import Data.Monoid
import Data.Typeable (Typeable)


newtype Script e r w s p a = Script
  { runScript :: (s,r) -> forall v. ((Either e a, s, w) -> v) -> (forall u. p u -> (u -> v) -> v) -> v
  } deriving Typeable


execScriptC
  :: s
  -> r
  -> ((Either e a, s, w) -> v)
  -> (forall u. p u -> (u -> v) -> v)
  -> Script e r w s p a
  -> v
execScriptC s r end cont x =
  runScript x (s,r) end cont


execScript
  :: s
  -> r
  -> (forall a. p a -> a)
  -> Script e r w s p t
  -> (Either e t, s, w)
execScript s r eval =
  execScriptC s r id (\p c -> c $ eval p)


execScriptM
  :: (Monad m)
  => s
  -> r
  -> (forall a. p a -> m a)
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



ask
  :: (Monoid w)
  => Script e r w s p r
ask = Script $ \(s,r) -> \end _ ->
  end (Right r, s, mempty)



local
  :: (r -> r)
  -> Script e r w s p a
  -> Script e r w s p a
local = transport



transport
  :: (r2 -> r1)
  -> Script e r1 w s p a
  -> Script e r2 w s p a
transport f x = Script $ \(s,r) -> \end cont ->
  runScript x (s, f r) end cont



reader
  :: (Monoid w)
  => (r -> a)
  -> Script e r w s p a
reader f = fmap f ask



get
  :: (Monoid w)
  => Script e r w s p s
get = Script $ \(s,_) -> \end _ ->
  end (Right s, s, mempty)



put
  :: (Monoid w)
  => s
  -> Script e r w s p ()
put s = Script $ \(_,_) -> \end _ ->
  end (Right (), s, mempty)



modify
  :: (Monoid w)
  => (s -> s)
  -> Script e r w s p ()
modify f = Script $ \(s,_) -> \end _ ->
  end (Right (), f s, mempty)



modify'
  :: (Monoid w)
  => (s -> s)
  -> Script e r w s p ()
modify' f = Script $ \(s,_) -> \end _ ->
  end (Right (), f $! s, mempty)



gets
  :: (Monoid w)
  => (s -> a)
  -> Script e r w s p a
gets f = Script $ \(s,_) -> \end _ ->
  end (Right (f s), s, mempty)



tell
  :: w
  -> Script e r w s p ()
tell w = Script $ \(s,_) -> \end _ ->
  end (Right (), s, w)



listen
  :: Script e r w s p a
  -> Script e r w s p (a,w)
listen x = Script $ \(r,s) -> \end cont ->
  runScript x (r,s)
    (\(y,s,w) -> end (fmap (,w) y, s, w)) cont



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



except
  :: (Monoid w)
  => Either e a
  -> Script e r w s p a
except z = Script $ \(s,_) -> \end _ ->
  end (z, s, mempty)



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



throw
  :: (Monoid w)
  => e
  -> Script e r w s p a
throw e = Script $ \(s,_) -> \end _ ->
  end (Left e, s, mempty)



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


prompt
  :: (Monoid w)
  => p a
  -> Script e r w s p a
prompt p = Script $ \(s,_) -> \end cont ->
  cont p (\a -> end (Right a, s, mempty))
