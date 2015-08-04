{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Container.MultiSet where

import qualified Data.MultiSet as M
import Data.Container.Base

instance Elementary M.MultiSet where
    null = M.null
    count = M.size
    empty = M.empty
    singleton = M.singleton

instance  Ord a => Decidable a M.MultiSet where
    elem = M.member

instance (Ord a, Ord b) => Mapable a b M.MultiSet where
    map = M.map

instance Ord a => Insertable a M.MultiSet where
    insert = M.insert

instance Ord a => Deletable a M.MultiSet where
    delete = M.delete

instance Ord a => Unitable a M.MultiSet where
    union = M.union

instance Ord a => Intersectible a M.MultiSet where
    intersection = M.intersection
