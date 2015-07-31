module Data.Container.List where

import Data.Container
import qualified Data.List as L

listAsMultisetIntersection :: Eq a => [a] -> [a] -> [a]
listAsMultisetIntersection _ [] = []
listAsMultisetIntersection [] _ = []
listAsMultisetIntersection xl@(x:xs) yl@(y:ys) = undefined --TODO

instance Elementary [] where
    null = L.null
    elem = L.elem
    empty = []
    singleton x = [x]

instance Insertable [] where
    insert = (:)

instance Deletable [] where
    delete = L.delete

instance Unitable [] where
    union = (++)

instance Intersectible [] where
    intersection = listAsMultisetIntersection

instance Fullable [] where
    full = L.repeat

