{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-missing-signatures #-}

module MultiTrieTest where

import Prelude hiding (lookup, null, repeat)
import Data.MultiTrie
import qualified Data.Map as M
import qualified Data.List as L
import Test.Framework

{-# ANN module "HLint: ignore Use camelCase" #-}

type TestMultiTrie = MultiTrie Char Int

-- | properties of the empty MT
test_empty =
    do
        assertBool  (L.null $ values u)
        assertBool  (M.null $ children u)
        assertBool  (null u)
        assertEqual u v
        assertBool  (null v)
        assertEqual u w
        assertBool  (null w)
        assertEqual u x
        assertBool  (null x)
        assertEqual y u
        assertBool  (null y)
        assertEqual z u
        assertBool  (null z)
    where
        u = empty :: TestMultiTrie
        v = leaf []
        w = union u u
        x = intersection u u
        y = lookup "abc" u
        z = replace "abc" u u

-- | properties of the singleton MT
test_singleton =
    do
        assertEqual (values u) [x]
        assertBool (M.null $ children u)
        assertBool (not $ null u)
        assertBool (not $ null v)
        assertEqual u v
        assertBool (null $ lookup "abc" u)
    where
        u = singleton x :: TestMultiTrie
        v = fromList [("", x)]
        x = 0

-- | adding one value to an empty MT must make a singleton
test_add_one_to_empty_is_singleton = assertEqual u v
    where
        u = add x empty :: TestMultiTrie
        v = singleton x :: TestMultiTrie
        x = 0
 
-- | a leaf MT must be equal to an MT composed elementwise
test_leaf_is_multiple_add = assertEqual u v
    where
        u = foldr add (empty :: TestMultiTrie) l
        v = leaf l
        l = [1..10]

-- | fromList makes the same leaf
test_leaf_equals_fromList = assertEqual u v
    where
        u = fromList $ map (\x -> ("", x)) l
        v = leaf l :: TestMultiTrie
        l = [1..10]

-- | the order of construction does not matter
test_eq = assertEqual u v
    where
        u = fromList l :: TestMultiTrie
        v = fromList $ reverse l :: TestMultiTrie
        l = [("", 0),
             ("a", 1), ("aa", 2), ("aaa", 3),
             ("b", 9), ("bb", 8), ("bbb", 7)]

-- | fetching the sub-MT works
test_lookup = assertEqual (lookup "ab" u) v
    where
        u = fromList p :: TestMultiTrie
        v = fromList q :: TestMultiTrie
        p = [("", 1), ("a", 2), ("ab", 3), ("ab", 4), ("abc", 5)]
        q = [("", 3), ("", 4), ("c", 5)]

-- | adding an empty child does not change the MT
