{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Container.MultisetFromList (MultisetFromList, listFromMultiset) where

import qualified Data.List as L
import Data.Container

newtype MultisetFromList a = MultisetFromList { listFromMultiset :: [a] } deriving (Show)

instance Elementary a MultisetFromList where
    null = L.null . listFromMultiset
    elem x = L.elem x . listFromMultiset
    empty = MultisetFromList []
    singleton x = MultisetFromList [x]

instance Insertable a MultisetFromList where
    insert x = MultisetFromList . (x:) . listFromMultiset

instance Eq a => Deletable a MultisetFromList where
    delete x = MultisetFromList . L.delete x . listFromMultiset

instance Unitable a MultisetFromList where
    union u v = MultisetFromList $ listFromMultiset u ++ listFromMultiset v

listAsMultisetIntersection :: Eq a => [a] -> [a] -> [a]
listAsMultisetIntersection [] _ = []
listAsMultisetIntersection (x:xs) ys = if x `L.elem` ys
    then x : listAsMultisetIntersection xs (L.delete x ys)
    else listAsMultisetIntersection xs ys

instance Eq a => Intersectible a MultisetFromList where
    intersection u v = MultisetFromList $ listAsMultisetIntersection (listFromMultiset u) (listFromMultiset v)

instance Fullable a MultisetFromList where
    full = MultisetFromList . L.repeat
