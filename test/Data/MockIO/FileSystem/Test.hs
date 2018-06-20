module Data.MockIO.FileSystem.Test (
    tests
) where

import Data.Proxy
  ( Proxy(..) )

import Test.Tasty
import Test.Tasty.QuickCheck

import Data.MockIO.FileSystem

tests :: Int -> TestTree
tests num =
  localOption (QuickCheckTests $ 20 * num) $
  testGroup "FileSystem"
    [ testProperty "(==) reflexive" $
        prop_filesystem_eq_reflexive pI
    , testProperty "(==) symmetric" $
        prop_filesystem_eq_symmetric pI
    , testProperty "(==) transitive" $
        prop_filesystem_eq_transitive pI
    , testProperty "id /= appendLines h lns" $
        prop_appendLines_not_equal pI
    , testProperty "fileExists h . writeLines h lns" $
        prop_writeLines_fileExists pI
    , testProperty "writeFile is a left zero" $
        prop_writeLines_lzero pI
    , testProperty "hasFile h lns $ writeLines h lns fs == True" $
        prop_hasFile_writeLines pI
    , testProperty "appendLines bs . writeLines as == writeLines (as ++ bs)" $
        prop_appendLines_writeLines pI
    , testProperty "deleteFile is idempotent" $
        prop_deleteFile_idempotent pI
    , testProperty "not (fileExists h $ deleteFile h x)" $
        prop_fileExists_deleteFile pI
    , testProperty "get line from deleted file" $
        prop_readLine_deleteFile pI
    , testProperty "get line from empty file" $
        prop_readLine_empty pI
    , testProperty "getLines / hasFile" $
        prop_hasFile_getLines pI
    , testProperty "readLine / writeLines" $
        prop_readLine_writeLines pI
    ]

-- Fake handle type
pI :: Proxy Int
pI = Proxy



prop_filesystem_eq_reflexive
  :: (Eq a) => Proxy a -> FileSystem a -> Bool
prop_filesystem_eq_reflexive _ x =
  x == x

prop_filesystem_eq_symmetric
  :: (Eq a) => Proxy a -> FileSystem a -> FileSystem a -> Bool
prop_filesystem_eq_symmetric _ x y =
  (x == y) == (y == x)

prop_filesystem_eq_transitive
  :: (Eq a) => Proxy a -> FileSystem a -> FileSystem a -> FileSystem a -> Bool
prop_filesystem_eq_transitive _ x y z =
  if (x == y) && (y == z) then x == z else True

prop_appendLines_not_equal
  :: (Eq a) => Proxy a -> a -> String -> FileSystem a -> Bool
prop_appendLines_not_equal _ h lns fs =
  fs /= appendLines h [lns] fs

prop_writeLines_lzero
  :: (Eq a) => Proxy a -> a -> [String] -> [String] -> FileSystem a -> Bool
prop_writeLines_lzero _ h ls ms fs =
  (writeLines h ls fs) == (writeLines h ls $ writeLines h ms fs)

prop_writeLines_fileExists
  :: (Eq a) => Proxy a -> a -> [String] -> FileSystem a -> Bool
prop_writeLines_fileExists _ h lns fs =
  fileExists h $ writeLines h lns fs

prop_hasFile_writeLines
  :: (Eq a) => Proxy a -> a -> [String] -> FileSystem a -> Bool
prop_hasFile_writeLines _ h lns fs =
  hasFile h lns $ writeLines h lns fs

prop_appendLines_writeLines
  :: (Eq a) => Proxy a -> a -> [String] -> [String] -> FileSystem a -> Bool
prop_appendLines_writeLines _ h as bs fs =
  (==)
    (appendLines h bs $ writeLines h as fs)
    (writeLines h (as ++ bs) fs)

prop_deleteFile_idempotent
  :: (Eq a) => Proxy a -> a -> FileSystem a -> Bool
prop_deleteFile_idempotent _ h fs =
  (deleteFile h fs) == (deleteFile h $ deleteFile h fs)

prop_fileExists_deleteFile
  :: (Eq a) => Proxy a -> a -> FileSystem a -> Bool
prop_fileExists_deleteFile _ h fs =
  not (fileExists h $ deleteFile h fs)

prop_readLine_deleteFile
  :: (Eq a) => Proxy a -> a -> Int -> FileSystem a -> Bool
prop_readLine_deleteFile _ h err fs =
  (Left err) == readLine err (err+1) h (deleteFile h fs)

prop_readLine_empty
  :: (Eq a) => Proxy a -> a -> Int -> FileSystem a -> Bool
prop_readLine_empty _ h err fs =
  (Left err) == readLine (err+1) err h (writeLines h [] fs)

prop_hasFile_getLines
  :: (Eq a) => Proxy a -> a -> [String] -> FileSystem a -> Bool
prop_hasFile_getLines _ h ls fs =
  case getLines h fs of
    Nothing -> not $ fileExists h fs
    Just ms -> hasFile h ms fs

prop_readLine_writeLines
  :: (Eq a) => Proxy a -> a -> String -> FileSystem a -> Bool
prop_readLine_writeLines _ h str fs =
  case readLine () () h $ writeLines h [str] fs of
    Left _ -> False
    Right (x,_) -> x == str
