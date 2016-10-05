{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-missing-signatures #-}

module MultiTrieTest where

import Prelude hiding (null, repeat, map)
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
        y = subnode "abc" u
        z = subnodeReplace "abc" u u
        t = fromList []

-- | properties of the singleton MT
test_singleton =
    do
        assertEqual (values u) [x]
        assertBool  (M.null $ children u)
        assertBool  (not $ null u)
        assertEqual 1 (size u)
        assertEqual u (fromList [("", x)])
        assertEqual u (addValue x empty)
        assertEqual u (union empty u)
        assertEqual u (intersection u u)
        assertBool  (null $ subnode "abc" u)
        assertEqual (subnodeDelete "" u) empty
        assertEqual u (subnodeDelete "abc" u)
    where
        u = singleton x :: TestMultiTrie
        x = 0

-- | properties of a leaf MT
test_leaf =
    do
        assertEqual l (values u)
        assertBool  (M.null $ children u)
        assertEqual (length l) (size u)
        assertEqual u (foldr addValue (empty :: TestMultiTrie) l)
        assertEqual u (fromList $ L.map (\a -> ("", a)) l)
        assertEqual (leaf $ 0 : l) (addValue 0 u)
        assertEqual u (intersection u u)
        assertEqual u (intersection u $ leaf [0..20])
        assertEqual u (union empty u)
        assertEqual u (union (leaf [1..5]) (leaf [6..10]))
        assertEqual u (subnodeReplace "abc" empty u)
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
        assertEqual u (subnode "" u)
        assertEqual empty (subnode "zzz" u)
        assertEqual (subnode "a" u) t
        assertEqual u (subnodeDelete "zzz" u)
        assertEqual v (subnodeDelete "a" u)
        assertEqual u (subnodeReplace "a" t u)
        assertEqual u (subnodeReplace "a" t v)
        assertEqual u (union v w)
        assertBool  (u /= (union u u))
        assertEqual empty (intersection v w)
        assertEqual w (intersection u w)
        assertEqual u (intersection u (union u u))
        assertEqual y (map (+1) u)
        assertEqual u (fromList $ toList u)
        assertBool  (listAsMultiSetEquals l $ toList u)
    where
        u = fromList l :: TestMultiTrie
        v = fromList p
        w = fromList q
        t = fromList $ L.map (\(_:ns, x) -> (ns, x)) q
        y = fromList $ L.map (\(ns, x) -> (ns, x + 1)) l
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
        assertEqual w (subnodeDelete "a" $ subnodeDelete "b" u)
        assertEqual w (intersection w u)
        assertEqual w (intersection u w)
    where
        u = repeat s l :: TestMultiTrie
        v = subnode "baabbab" u
        w = leaf l
        l = [0, 1]
        s = ['a', 'b']

-- | map a function over a multi-trie
test_mtmap =
    do
        assertEqual v (map f u)
        assertEqual w (mapWithName g u)
    where
        u = fromList p :: TestMultiTrie
        v = fromList q
        w = fromList r
        p = [("", 1), ("abc", 2), ("a", 3), ("", 4),
                ("ab", 5), ("b", 6), ("bc", 7)]
        q = L.map (\(n, x) -> (n, f x)) p
        r = L.map (\(n, x) -> (n, g n x)) p
        f = (+7) . (*13)
        g n x = (fromIntegral $ L.length n) + x

-- | union, intersection and cartesian product
test_binop =
    do
        assertEqual w (union u v)
        assertEqual v (union empty v)
        assertEqual u (union u empty)
        assertEqual x (intersection u v)
        assertBool  (null $ intersection u empty)
        assertBool  (null $ intersection empty v)
        assertEqual y (cartesian u v)
        assertBool  (null $ cartesian u empty)
        assertBool  (null $ cartesian empty v)
        assertEqual u (map snd (cartesian z u))
        assertEqual u (map fst (cartesian u z))
    where
        u = fromList p :: TestMultiTrie
        v = fromList q
        w = fromList (p ++ q)
        x = fromList (L.intersect p q)
        y = fromList (listProduct (toList u) (toList v))
        z = leaf [()]
        p = [("", 1), ("abc", 2), ("a", 3), ("", 4),
                ("ab", 5), ("b", 6), ("bc", 7)]
        q = [("pqr", 9), ("ac", 8), ("bc", 7), ("", 6),
                ("", 4), ("abc", 3), ("abc", 2), ("p", 1)]

test_flatten =
    do
        assertEqual u (flatten v)
    where
        u = fromList p :: TestMultiTrie
        v = fromList q
        p = [(n1 ++ n2, x2) | (n1, l1) <- r, (n2, x2) <- l1]
        q = L.map (\(n, l) -> (n, fromList l)) r
        r = [
                ("", [("", 0), ("ab", 1), ("abcba", 2), ("", 3), ("abc", 4)]),
                ("ab", [("c", 1), ("", 2), ("b", 3), ("cba", 4)]),
                ("abcb", []),
                ("abc", [("", 2), ("b", 1), ("ba", 0)])
            ]

listProduct l1 l2 = [(n1 ++ n2, (v1, v2)) | (n1, v1) <- l1, (n2, v2) <- l2] 
