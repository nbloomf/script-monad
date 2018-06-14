{- |
Module      : Data.MockIO
Description : A mock IO monad for testing.
Copyright   : 2018, Automattic, Inc.
License     : BSD3
Maintainer  : Nathan Bloomfield (nbloomf@gmail.com)
Stability   : experimental
Portability : POSIX

A fake filesystem for testing.
-}

{-# LANGUAGE ScopedTypeVariables #-}
module Data.MockIO.FileSystem (
    FileSystem(..)
  , File(..)
  , emptyFileSystem
  , fileExists
  , hasFile
  , deleteFile
  , getLines
  , writeLines
  , appendLines
  , readLine
) where

import Data.Maybe
import Data.List

import Test.QuickCheck
  ( Arbitrary(..), Positive(..), Gen, vectorOf )



-- | Abstraction of a text file consisting of a "handle" and a list of lines.
data File a = File
  { _fileHandle :: a -- ^ File "handle"
  , _fileContents :: [String] -- ^ List of lines
  } deriving Eq

instance (Show a) => Show (File a) where
  show (File h lns) = unlines $
    [ ">>>>> " ++ show h ++ ":" ] ++ lns ++ ["<<<<<"]





-- | A mapping from "handles" of type @a@ to lists of lines.
data FileSystem a = FileSystem [File a]

instance (Eq a) => Eq (FileSystem a) where
  (FileSystem as) == (FileSystem bs) = and
    [ all (`elem` bs) as
    , all (`elem` as) bs
    ]

instance (Show a) => Show (FileSystem a) where
  show (FileSystem fs) = concatMap show fs

instance (Eq a, Arbitrary a) => Arbitrary (FileSystem a) where
  arbitrary = do
    Positive n <- arbitrary :: Gen (Positive Int)
    handles <- fmap nub $ vectorOf (n `mod` 20) arbitrary
    FileSystem <$> mapM (\k -> File k <$> arbitrary ) handles

-- | No files; populate with `writeLines` or `appendLines`.
emptyFileSystem :: FileSystem a
emptyFileSystem = FileSystem []





getFile :: (Eq a) => a -> FileSystem a -> Maybe (File a)
getFile h (FileSystem fs) = lookup fs
  where
    lookup zs = case zs of
      [] -> Nothing
      f:rest -> if h == _fileHandle f
        then Just f
        else lookup rest

putFile :: (Eq a) => File a -> FileSystem a -> FileSystem a
putFile f (FileSystem fs) = FileSystem $ putFile' fs
  where
    putFile' zs = case zs of
      [] -> [f]
      (g:rest) -> if _fileHandle f == _fileHandle g
        then f : rest
        else g : putFile' rest

-- | Detect whether a file with the given handle exists.
fileExists
  :: (Eq a)
  => a -- ^ File handle
  -> FileSystem a
  -> Bool
fileExists h = isJust . getFile h

-- | Detect whether a file with the given handle exists and has given contents.
hasFile
  :: (Eq a)
  => a -- ^ Handle
  -> [String] -- ^ Contents
  -> FileSystem a
  -> Bool
hasFile h lns fs = case getLines h fs of
  Nothing -> False
  Just ms -> ms == lns

-- | Retrieve the contents of a file, or nothing if the file does not exist.
getLines
  :: (Eq a)
  => a -- ^ Handle
  -> FileSystem a
  -> Maybe [String]
getLines h = fmap _fileContents . getFile h

-- | Overwrite the contents of a file.
writeLines
  :: (Eq a)
  => a -- ^ Handle
  -> [String] -- ^ Contents
  -> FileSystem a
  -> FileSystem a
writeLines a lns = putFile (File a lns)

-- | Append to a file.
appendLines
  :: (Eq a)
  => a -- ^ Handle
  -> [String] -- ^ Contents
  -> FileSystem a
  -> FileSystem a
appendLines h ls (FileSystem fs) = FileSystem $ appendLines' fs
  where
    appendLines' zs = case zs of
      [] -> [File h ls]
      (File u ms):rest -> if u == h
        then (File u (ms ++ ls)) : rest
        else (File u ms) : appendLines' rest

-- | Delete a file; if no such file exists, has no effect.
deleteFile
  :: (Eq a)
  => a -- ^ Handle
  -> FileSystem a
  -> FileSystem a
deleteFile h (FileSystem fs) = FileSystem $ deleteFile' fs
  where
    deleteFile' zs = case zs of
      [] -> []
      m:rest -> if h == _fileHandle m
        then rest
        else m : deleteFile' rest

-- | Read the first line of a file.
readLine
  :: (Eq a)
  => e -- ^ Handle not found error
  -> e -- ^ EOF error
  -> a -- ^ Handle
  -> FileSystem a
  -> Either e (String, FileSystem a)
readLine notFound eof k (FileSystem fs) = getline fs []
  where
    getline xs ys = case xs of
      [] -> Left notFound
      (File u x):rest -> if k == u
        then case x of
          [] -> Left eof
          w:ws -> Right (w, FileSystem $ [File k ws] ++ rest ++ ys)
        else getline rest ((File u x):ys)
