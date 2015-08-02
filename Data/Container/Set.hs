{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Container.Set where

import qualified Data.Set as S
import Data.Container.Base
import Data.Allable

instance Ord a => Elementary a S.Set where
    null = S.null
    elem = S.member
    empty = S.empty
    singleton = S.singleton

instance Ord a => Insertable a S.Set where
    insert = S.insert

instance Ord a => Deletable a S.Set where
    delete = S.delete

instance Ord a => Unitable a S.Set where
    union = S.union

instance Ord a => Intersectible a S.Set where
    intersection = S.intersection

instance Ord a => Repeatable a S.Set where
    repeat = S.singleton

instance (Enum a, Bounded a, Ord a) => Topable a S.Set where
    top = S.fromDistinctAscList allValues
