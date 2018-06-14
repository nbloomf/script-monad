{- |
Module      : Data.Aeson.Extras
Description : Some stuff not included in Data.Aeson.
Copyright   : 2018, Automattic, Inc.
License     : BSD3
Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
Stability   : experimental
Portability : POSIX

JSON helpers.
-}

module Data.Aeson.Extras (
    JsonError(..)
) where

import Data.Aeson
  ( Value() )
import Data.ByteString.Lazy
  ( ByteString )
import Data.String
  ( fromString )
import Data.Text
  ( Text )
import Test.QuickCheck
  ( Arbitrary(..), Gen )



-- | Represents the kinds of errors that can occur when parsing and decoding JSON.
data JsonError
  -- | A generic JSON error; try not to use this.
  = JsonError String
  -- | A failed parse.
  | JsonParseError ByteString
  -- | An attempt to look up the value of a key that does not exist on an object.
  | JsonKeyDoesNotExist Text Value
  -- | An attempt to look up the value of a key on something other than an object.
  | JsonKeyLookupOffObject Text Value
  -- | A failed attempt to convert a `Value` to some other type.
  | JsonConstructError String
  deriving (Eq, Show)

instance Arbitrary JsonError where
  arbitrary = do
    k <- arbitrary :: Gen Int
    case k `mod` 5 of
      0 -> JsonError <$> arbitrary
      1 -> JsonParseError <$> (fromString <$> arbitrary)
      2 -> JsonKeyDoesNotExist <$>
             (fromString <$> arbitrary) <*>
             (fromString <$> arbitrary)
      3 -> JsonKeyLookupOffObject <$>
             (fromString <$> arbitrary) <*>
             (fromString <$> arbitrary)
      _ -> JsonConstructError <$> arbitrary
