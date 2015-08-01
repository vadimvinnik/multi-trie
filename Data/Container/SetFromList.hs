module Data.Container.SetFromList where

import qualified Data.List as L
import Data.Container
import Data.Allable

newtype SetFromList a = SetFromList { listFromSet :: [a] }

instance Elementary SetFromList where
    null = L.null . listFromSet
    elem x = L.elem x . listFromSet
    empty = SetFromList []
    singleton x = SetFromList [x]

insertUnique :: Eq a => a -> [a] -> [a]
insertUnique x xs = if x `L.elem` xs then xs else x:xs

instance Insertable SetFromList where
    insert x = SetFromList . insertUnique x . listFromSet

instance Deletable SetFromList where
    delete x = SetFromList . L.delete x . listFromSet

instance Unitable SetFromList where
    union u v = SetFromList $ L.union (listFromSet u) (listFromSet v)

instance Intersectible SetFromList where
    intersection u v = SetFromList $ L.intersect (listFromSet u) (listFromSet v)

instance Fullable SetFromList where
    full = SetFromList . L.repeat

instance Topable SetFromList where
    top = SetFromList allValues
