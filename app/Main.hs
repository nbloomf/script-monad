module Main where

import System.IO hiding (putStrLn)
import Control.Monad.Script.Http

import Prelude hiding (putStrLn)

import Control.Concurrent (newMVar)

main :: IO ()
main = do
  lock <- newMVar ()
  (result,_,_) <- doit lock test
  return ()

data Q a = Q a

evalQ :: Q a -> IO a
evalQ (Q a) = return a

test :: Http () () () () Q ()
test = do
  comment "howdy"
  wait 500000
  putStrLn "woo"
  httpGet "https://google.com"
  return ()

doit lock = execHttpM (basicState ()) (basicEnv lock (const show) (const show) ()) (evalIO evalQ)