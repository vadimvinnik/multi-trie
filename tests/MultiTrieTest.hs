{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-missing-signatures #-}

module MultiTrieTest where

import Data.MultiTrie
import qualified Data.Map as M
import qualified Data.List as L
import Test.Framework

{-# ANN module "HLint: ignore Use camelCase" #-}

type TestMultiTrie = MultiTrie String Int

test_empty =
    do
        assertBool (L.null $ values u)
        assertBool (M.null $ children u)
    where
        u = empty :: TestMultiTrie

test_singleton =
    do
        assertEqual (values u) [x]
        assertBool (M.null $ children u)
    where
        u = singleton x :: TestMultiTrie
        x = 0

{-
test_add_single = assertEqual (add x u) v
    where
        u = empty :: TestMultiTrie
        v = singleton x :: TestMultiTrie
        x = 0
 
test_add_multiple = assertEqual u (MultiTrie l M.empty)
    where
        u = foldr add (empty :: TestMultiTrie) l
        l = [1..10]
-}
