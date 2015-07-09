{-# OPTIONS_GHC -F -pgmF htfpp #-}
module MultiTrieTest where

import MultiTrie
import qualified Data.Map.Lazy as M
import Test.Framework

type TestMultiTrie = MultiTrie String Int

test_empty =
    do
        assertEqual (values m) []
        assertEqual (M.toList $ children m) []
    where
        m = empty :: TestMultiTrie

test_singleton =
    do
        assertEqual (values m) [x]
        assertEqual (M.toList $ children m) []
    where
        m = singleton x :: TestMultiTrie
        x = 0
