{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-missing-signatures #-}

module MultiTrieTest where

import Data.MultiTrie
import qualified Data.Map.Lazy as M
import Test.Framework

{-# ANN module "HLint: ignore Use camelCase" #-}

type TestMultiTrie = MultiTrie String Int

test_empty =
    do
        assertEqual (values u) []
        assertEqual (M.toList $ children u) []
    where
        u = empty :: TestMultiTrie

test_singleton =
    do
        assertEqual (values u) [x]
        assertEqual (children u) M.empty
    where
        u = singleton x :: TestMultiTrie
        x = 0

test_put_single = assertEqual (put x u) v
    where
        u = empty :: TestMultiTrie
        v = singleton x :: TestMultiTrie
        x = 0
 
test_put_multiple = assertEqual u (MultiTrie l M.empty)
    where
        u = foldr put (empty :: TestMultiTrie) l
        l = [1..10]
