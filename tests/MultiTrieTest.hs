{-# OPTIONS_GHC -F -pgmF htfpp #-}
module MultiTrieTest where
import MultiTrie


import Test.Framework

test_succeeds =  assertBool True

test_fails = assertBoolVerbose "Must fail" False