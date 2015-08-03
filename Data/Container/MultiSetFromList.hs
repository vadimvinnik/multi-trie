{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Container.MultiSetFromList (MultiSetFromList, listFromMultiSet) where

import qualified Data.List as L
import Data.Container.Base

newtype MultiSetFromList a = MultiSetFromList { listFromMultiSet :: [a] } deriving (Show)

instance Elementary a MultiSetFromList where
    null = L.null . listFromMultiSet
    elem x = L.elem x . listFromMultiSet
    count = L.length . listFromMultiSet
    empty = MultiSetFromList []
    singleton x = MultiSetFromList [x]

instance Mapable a b MultiSetFromList where
    map f = MultiSetFromList . L.map f . listFromMultiSet

instance Insertable a MultiSetFromList where
    insert x = MultiSetFromList . (x:) . listFromMultiSet

instance Eq a => Deletable a MultiSetFromList where
    delete x = MultiSetFromList . L.delete x . listFromMultiSet

instance Unitable a MultiSetFromList where
    union u v = MultiSetFromList $ listFromMultiSet u ++ listFromMultiSet v

listAsMultiSetIntersection :: Eq a => [a] -> [a] -> [a]
listAsMultiSetIntersection [] _ = []
listAsMultiSetIntersection (x:xs) ys = if x `L.elem` ys
    then x : listAsMultiSetIntersection xs (L.delete x ys)
    else listAsMultiSetIntersection xs ys

instance Eq a => Intersectible a MultiSetFromList where
    intersection u v = MultiSetFromList $ listAsMultiSetIntersection (listFromMultiSet u) (listFromMultiSet v)
