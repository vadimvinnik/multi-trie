{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Container.SetFromList (SetFromList, listFromSet) where

import qualified Data.List as L
import Data.Container
import Data.Allable

newtype SetFromList a = SetFromList { listFromSet :: [a] } deriving (Show)

instance Elementary a SetFromList where
    null = L.null . listFromSet
    elem x = L.elem x . listFromSet
    empty = SetFromList []
    singleton x = SetFromList [x]

insertUnique :: Eq a => a -> [a] -> [a]
insertUnique x xs = if x `L.elem` xs then xs else x:xs

instance Eq a => Insertable a SetFromList where
    insert x = SetFromList . insertUnique x . listFromSet

instance Eq a => Deletable a SetFromList where
    delete x = SetFromList . L.delete x . listFromSet

instance Eq a => Unitable a SetFromList where
    union u v = SetFromList $ L.union (listFromSet u) (listFromSet v)

instance Eq a => Intersectible a SetFromList where
    intersection u v = SetFromList $ L.intersect (listFromSet u) (listFromSet v)

instance Fullable a SetFromList where
    full = SetFromList . L.repeat

instance (Enum a, Bounded a) => Topable a SetFromList where
    top = SetFromList allValues
