{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-missing-signatures #-}

module MultiTrieTest where

import Prelude hiding (lookup, null, repeat)
import Data.MultiTrie
import Data.Int
import qualified Data.Map as M
import qualified Data.List as L
import Test.Framework

{-# ANN module "HLint: ignore Use camelCase" #-}

type TestMultiTrie = MultiTrie Char Int8

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
        assertEqual y (mtmap (+1) u)
    where
        u = fromList l :: TestMultiTrie
        v = fromList p
        w = fromList q
        t = fromList $ map (\(_:ns, x) -> (ns, x)) q
        y = fromList $ map (\(ns, x) -> (ns, x + 1)) l
        l = p ++ q
        p = [("", 0), ("b", 9), ("", 1), ("b", 8), ("", 2), ("b", 7)]
        q = [("a", 1), ("aa", 2), ("ab", 3), ("aaa", 4), ("aba", 5)]

-- | properties of an infinite MT
test_repeat =
    do
        assertBool  (not $ null u)
        assertEqual l (values u)
        assertEqual s (M.keys $ children u)
        assertEqual l (values v)
        assertEqual s (M.keys $ children v)
        assertEqual w (delete "a" $ delete "b" u)
        assertEqual w (intersection w u)
        assertEqual w (intersection u w)
    where
        u = repeat s l :: TestMultiTrie
        v = lookup "baabbab" u
        w = leaf l
        l = [0, 1]
        s = ['a', 'b']

-- | properties of the largest MT
test_top =
    do
        assertEqual v (intersection u v)
        assertEqual v (intersection v u)
        assertEqual q (M.keys $ children u)
        assertEqual q (M.keys $ children w)
        assertEqual (f r) (f $ values u)
        assertEqual (f r) (f $ values w)
    where
        u = top :: MultiTrie Ordering Bool
        v = fromList $
            [
                ([EQ], True),
                ([EQ, GT], False),
                ([EQ, GT], True),
                ([LT, GT, EQ, LT, GT], False),
                ([LT, GT, EQ, GT, GT], False)
            ]
        w = lookup p u
        p = [LT, GT, EQ, GT, GT]
        q = [LT, EQ, GT]
        r = cycle [False, True]
        f = take 20

-- | union, intersection and cartesian product
test_binop =
    do
        assertEqual w (union u v)
        assertEqual x (intersection u v)
        assertEqual y (cartesianProduct u v)
    where
        u = fromList p :: TestMultiTrie
        v = fromList q
        w = fromList (p ++ q)
        x = fromList (L.intersect p q)
        y = fromList (listProduct p q)
        p = [("", 1), ("abc", 2), ("a", 3), ("", 4), ("ab", 5), ("b", 6), ("bc", 7)]
        q = [("pqr", 9), ("ac", 8), ("bc", 7), ("", 6), ("", 4), ("abc", 3), ("abc", 2), ("p", 1)]

listProduct l1 l2 = [(n1 ++ n2, (v1, v2)) | (n1, v1) <- l1, (n2, v2) <- l2] 
