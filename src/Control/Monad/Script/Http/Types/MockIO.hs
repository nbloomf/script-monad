module Control.Monad.Script.Http.Types.MockIO (
    MockIO(..)
  , evalMockIO
) where

import Control.Monad.Script.Http.Types (P(..))

data MockIO a = MockIO a

evalMockIO :: (p a -> MockIO a) -> P p a -> MockIO a
evalMockIO = undefined
