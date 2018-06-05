{- |
Module      : Control.Monad.Script.Http
Description : A generic monad for expressing HTTP interactions.
Copyright   : 2018, Automattic, Inc.
License     : BSD3
Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
Stability   : experimental
Portability : POSIX

A basic type and monad for describing HTTP interactions.
-}

{-# LANGUAGE Rank2Types, RecordWildCards, NoImplicitPrelude #-}
module Control.Monad.Script.Http (
    Http()
  , execHttpM

  , ask
  , local
  , reader

  , get
  , put
  , modify
  , modify'
  , gets

  , listen
  , pass
  , censor

  , except
  , throw
  , catch

  , prompt

  , E(..)
  , R(..)
  , basicEnv
  , S(..)
  , basicState
  , W(..)

  , Url
  , Color(..)

  , comment
  , wait

  , throwError

  , httpGet
  , httpSilentGet
  , httpPost
  , httpSilentPost
  , httpDelete
  , httpSilentDelete

  , putStrLn
  , getLine

  , evalIO

  , MockIO(..)
  , evalMockIO
) where

import Control.Monad (Functor(..), Monad(..), ap)
import Control.Applicative (Applicative(..))
import Data.ByteString.Lazy (ByteString)
import Data.Either (Either(..))
import Data.Function (($), (.))
import Data.Int (Int)
import Data.Maybe (Maybe(..))
import Data.String (String)
import Data.Typeable (Typeable)

import qualified Control.Monad.Script as S

import Control.Monad.Script.Http.Types
import Control.Monad.Script.Http.Types.IO
import Control.Monad.Script.Http.Types.MockIO



-- | An HTTP session returning an @a@, writing to a log of type @W e w@, reading from an environment of type @R e w r@, with state of type @S s@, throwing errors of type @E e@, and performing effectful computations described by @P p a@.
--
-- Behind the scenes @Http@ is a stack of reader, writer, state, error, and prompt monads.
newtype Http e r w s p a = Http
  { http :: S.Script (E e) (R e w r) (W e w) (S s) (P p) a
  } deriving Typeable



instance Functor (Http e r w s p) where
  fmap f = Http . fmap f . http

instance Applicative (Http e r w s p) where
  pure = return
  (<*>) = ap

instance Monad (Http e r w s p) where
  return = Http . return
  (Http x) >>= f = Http (x >>= (http . f))



execHttpM
  :: (Monad m)
  => S s
  -> R e w r
  -> (forall a. P p a -> m a)
  -> Http e r w s p t
  -> m (Either (E e) t, S s, W e w)
execHttpM s r p = S.execScriptM s r p . http



ask
  :: Http e r w s p (R e w r)
ask = Http S.ask

local
  :: (R e w r -> R e w r)
  -> Http e r w s p a
  -> Http e r w s p a
local f = Http . S.local f . http

transport
  :: (R e w r2 -> R e w r1)
  -> Http e r1 w s p a
  -> Http e r2 w s p a
transport f = Http . S.transport f . http

reader
  :: (R e w r -> a)
  -> Http e r w s p a
reader f = Http (S.reader f)

get
  :: Http e r w s p (S s)
get = Http S.get

put
  :: S s
  -> Http e r w s p ()
put s = Http (S.put s)

modify
  :: (S s -> S s)
  -> Http e r w s p ()
modify f = Http (S.modify f)

modify'
  :: (S s -> S s)
  -> Http e r w s p ()
modify' f = Http (S.modify' f)

gets
  :: (S s -> a)
  -> Http e r w s p a
gets f = Http (S.gets f)

tell
  :: W e w
  -> Http e r w s p ()
tell w = Http (S.tell w)

listen
  :: Http e r w s p a
  -> Http e r w s p (a, W e w)
listen = Http . S.listen . http

pass
  :: Http e r w s p (a, W e w -> W e w)
  -> Http e r w s p a
pass = Http . S.pass . http

censor
  :: (W e w -> W e w)
  -> Http e r w s p a
  -> Http e r w s p a
censor f = Http . S.censor f . http

except
  :: Either (E e) a
  -> Http e r w s p a
except e = Http (S.except e)

throw
  :: E e
  -> Http e r w s p a
throw e = Http (S.throw e)

catch
  :: Http e r w s p a
  -> (E e -> Http e r w s p a)
  -> Http e r w s p a
catch x f = Http (S.catch (http x) (http . f))

prompt
  :: P p a
  -> Http e r w s p a
prompt p = Http (S.prompt p)



logNow
  :: Log e w
  -> Http e r w s p ()
logNow msg = do
  time <- prompt $ GetSystemTime
  R{..} <- ask
  case printLogWith _logOptions (time,_uid,msg) of
    Nothing -> return ()
    Just str -> prompt $ HPutStrLnBlocking _logLock _logHandle str
  tell $ W [(time, msg)]


throwError
  :: E e
  -> Http e r w s p a
throwError e = do
  logNow $ errorMessage e
  throw e


comment
  :: String
  -> Http e r w s p ()
comment msg = do
  logNow $ L_Comment msg


httpGet
  :: Url
  -> Http e r w s p HttpResponse
httpGet url = do
  R{..} <- ask
  S{..} <- get
  logNow $ L_Request GET url _httpOptions Nothing
  result <- prompt $ HttpGet _httpOptions _httpSession url
  case result of
    Right response -> do
      logNow $ L_Response response
      return response
    Left err -> case _httpErrorInject err of
      Just z -> throwError $ E z
      Nothing -> throwError $ E_Http err


httpSilentGet
  :: Url
  -> Http e r w s p HttpResponse
httpSilentGet url = do
  R{..} <- ask
  S{..} <- get
  logNow $ L_SilentRequest
  result <- prompt $ HttpGet _httpOptions _httpSession url
  case result of
    Right response -> do
      logNow $ L_SilentResponse
      return response
    Left err -> case _httpErrorInject err of
      Just z -> throwError $ E z
      Nothing -> throwError $ E_Http err


httpPost
  :: Url
  -> ByteString
  -> Http e r w s p HttpResponse
httpPost url payload = do
  R{..} <- ask
  S{..} <- get
  logNow $ L_Request POST url _httpOptions (Just payload)
  result <- prompt $ HttpPost _httpOptions _httpSession url payload
  case result of
    Right response -> do
      logNow $ L_Response response
      return response
    Left err -> case _httpErrorInject err of
      Just z -> throwError $ E z
      Nothing -> throwError $ E_Http err


httpSilentPost
  :: Url
  -> ByteString
  -> Http e r w s p HttpResponse
httpSilentPost url payload = do
  R{..} <- ask
  S{..} <- get
  logNow $ L_SilentRequest
  result <- prompt $ HttpPost _httpOptions _httpSession url payload
  case result of
    Right response -> do
      logNow $ L_SilentResponse
      return response
    Left err -> case _httpErrorInject err of
      Just z -> throwError $ E z
      Nothing -> throwError $ E_Http err



httpDelete
  :: Url
  -> Http e r w s p HttpResponse
httpDelete url = do
  R{..} <- ask
  S{..} <- get
  logNow $ L_Request DELETE url _httpOptions Nothing
  result <- prompt $ HttpDelete _httpOptions _httpSession url
  case result of
    Right response -> do
      logNow $ L_Response response
      return response
    Left err -> case _httpErrorInject err of
      Just z -> throwError $ E z
      Nothing -> throwError $ E_Http err


httpSilentDelete
  :: Url
  -> Http e r w s p HttpResponse
httpSilentDelete url = do
  R{..} <- ask
  S{..} <- get
  logNow $ L_SilentRequest
  result <- prompt $ HttpDelete _httpOptions _httpSession url
  case result of
    Right response -> do
      logNow $ L_SilentResponse
      return response
    Left err -> case _httpErrorInject err of
      Just z -> throwError $ E z
      Nothing -> throwError $ E_Http err


putStrLn
  :: String
  -> Http e r w s p ()
putStrLn string = do
  h <- reader _stdout
  prompt $ HPutStrLn h string
  return ()

getLine
  :: Http e r w s p String
getLine = do
  h <- reader _stdin
  prompt $ HGetLine h


wait
  :: Int
  -> Http e r w s p ()
wait k = do
  logNow $ L_Pause k
  prompt $ ThreadDelay k

