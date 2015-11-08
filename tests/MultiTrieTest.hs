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
        assertEqual 0 (size u)
        assertBool  (null u)
        assertEqual u v
        assertBool  (null v)
        assertEqual u w
        assertBool  (null w)
        assertEqual u x
        assertBool  (null x)
        assertEqual u y
        assertBool  (null y)
        assertEqual u z
        assertBool  (null z)
        assertEqual u t
        assertBool  (null t)
    where
        u = empty :: TestMultiTrie
        v = leaf []
        w = union u u
        x = intersection u u
        y = lookup "abc" u
        z = replace "abc" u u
        t = fromList []

-- | properties of the singleton MT
test_singleton =
    do
        assertEqual (values u) [x]
        assertBool  (M.null $ children u)
        assertBool  (not $ null u)
        assertEqual 1 (size u)
        assertEqual u (fromList [("", x)])
        assertEqual u (add x empty)
        assertEqual u (union empty u)
        assertEqual u (intersection u u)
        assertBool  (null $ lookup "abc" u)
        assertEqual (delete "" u) empty
        assertEqual u (delete "abc" u)
    where
        u = singleton x :: TestMultiTrie
        x = 0

-- | properties of a leaf MT
test_leaf =
    do
        assertEqual l (values u)
        assertBool  (M.null $ children u)
        assertEqual (length l) (size u)
        assertEqual u (foldr add (empty :: TestMultiTrie) l)
        assertEqual u (fromList $ map (\a -> ("", a)) l)
        assertEqual (leaf $ 0 : l) (add 0 u)
        assertEqual u (intersection u u)
        assertEqual u (intersection u $ leaf [0..20])
        assertEqual u (union empty u)
        assertEqual u (union (leaf [1..5]) (leaf [6..10]))
        assertEqual u (replace "abc" empty u)
    where
        u = leaf l :: TestMultiTrie
        l = [1..10]

-- | basic properties of a general case MT
test_general_basic =
    do
        assertBool  (not $ null u)
        assertEqual [0, 1, 2] (values u)
        assertEqual ['a', 'b'] (M.keys $ children u)
        assertEqual (length l) (size u)
        assertEqual u (fromList $ q ++ p)
        assertEqual u (lookup "" u)
        assertEqual empty (lookup "zzz" u)
        assertEqual (lookup "a" u) t
        assertEqual u (delete "zzz" u)
        assertEqual v (delete "a" u)
        assertEqual u (replace "a" t u)
        assertEqual u (replace "a" t v)
        assertEqual u (union v w)
        assertBool  (u /= (union u u))
        assertEqual empty (intersection v w)
        assertEqual w (intersection u w)
        assertEqual u (intersection u (union u u))
    where
        u = fromList l :: TestMultiTrie
        v = fromList p
        w = fromList q
        t = fromList r
        l = p ++ q
        p = [("", 0), ("b", 9), ("", 1), ("b", 8), ("", 2), ("b", 7)]
        q = [("a", 1), ("aa", 2), ("ab", 3), ("aaa", 4), ("aba", 5)]
        r = map (\(_:ns, x) -> (ns, x)) q

