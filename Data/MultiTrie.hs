{-# LANGUAGE FlexibleContexts #-}

module Data.MultiTrie where

import Prelude hiding (lookup, map, null, repeat)
import qualified Data.Foldable as F
import qualified Data.Map as M
import qualified Data.Tree as T
import qualified Data.List as L
import Control.Applicative hiding (empty)

import Data.Allable

type MultiTrieMap n v c = M.Map n (MultiTrie n v c) 

data MultiTrie n v c = MultiTrie
    {
        values :: c v,
        children :: MultiTrieMap n v c
    }
    deriving (Eq, Show)

empty :: MultiTrie n v c
empty = MultiTrie [] M.empty

singleton :: v -> MultiTrie n v c
singleton x = MultiTrie [x] M.empty

repeat :: Ord n => [n] -> [v] -> MultiTrie n v c
repeat ns xs = MultiTrie xs (M.fromList $ zip ns $ L.repeat $ repeat ns xs)

top :: (Ord n, Bounded n, Enum n, Bounded v, Enum v) => MultiTrie n v c
top = repeat allValues $ L.cycle allValues

null :: MultiTrie n v c -> Bool
null (MultiTrie vs m) = null vs && L.all null (M.elems m)

size :: MultiTrie n v c -> Int
size (MultiTrie vs m) = L.length vs + L.sum (L.map size (M.elems m))

lookup :: Ord n => [n] -> MultiTrie n v c -> MultiTrie n v c
lookup [] mt = mt
lookup (n:ns) (MultiTrie _ m) = maybe empty (lookup ns) (M.lookup n m)

fetch :: Ord n => [n] -> MultiTrie n v c -> c v
fetch ns = values . lookup ns

update :: Ord n => [n] -> (MultiTrie n v c -> MultiTrie n v c) -> MultiTrie n v c -> MultiTrie n v c
update [] f mt = f mt
update (n:ns) f (MultiTrie vs m) = MultiTrie vs (M.alter (toMaybe . update ns f . fromMaybe) n m)

put :: v -> MultiTrie n v c -> MultiTrie n v c
put x (MultiTrie vs m) = MultiTrie x:vs m

insert :: Ord n => [n] -> v -> MultiTrie n v c -> MultiTrie n v c
insert ns v = update ns (put v)

replace :: Ord n => [n] -> MultiTrie n v c -> MultiTrie n v c -> MultiTrie n v c
replace ns mt1 = update ns (const mt1)

delete :: Ord n => [n] -> MultiTrie n v c -> MultiTrie n v c
delete ns = replace ns empty

unite :: Ord n => [n] -> MultiTrie n v c -> MultiTrie n v c -> MultiTrie n v c
unite ns mt1 = update ns (union mt1)

intersect :: Ord n => [n] -> MultiTrie n v c -> MultiTrie n v c -> MultiTrie n v c
intersect ns mt1 = update ns (intersection mt1)

map :: Ord n => (v -> w) -> MultiTrie n v c -> MultiTrie n w c
map f = mapContainers (L.map f)

mapWithName :: Ord n => ([n] -> v -> w) -> MultiTrie n v c -> MultiTrie n w c
mapWithName f = mapContainersWithName (L.map . f) 

mapAll :: (Ord n, Applicative c) => c (v -> w) -> MultiTrie n v c -> MultiTrie n w c
mapAll fs  = mapContainers (fs <*>)

mapAllWithName :: (Ord n, Applicative c) => c ([n] -> v -> w) -> MultiTrie n v c -> MultiTrie n w c
mapAllWithName fs = mapContainersWithName (\ns -> (L.map ($ns) fs <*>))

mapContainers :: Ord n => (c v -> c w) -> MultiTrie n v c -> MultiTrie n w c
mapContainers fl (MultiTrie vs vm) = MultiTrie (fl vs) (M.map (mapContainers fl) vm)

mapContainersWithName :: Ord n => ([n] -> c v -> c w) -> MultiTrie n v c -> MultiTrie n w c
mapContainersWithName fl (MultiTrie vs vm) = MultiTrie (fl [] vs) (M.mapWithKey (\n -> mapContainersWithName $ fl . (n:)) vm)

cartesianProduct :: Ord n => MultiTrie n v c -> MultiTrie n w c -> MultiTrie n (v, w) c
cartesianProduct mtv = applyCartesian (map (,) mtv)

-- TODO
union :: Ord n => MultiTrie n v c -> MultiTrie n v c -> MultiTrie n v c
union = zipContentsAndChildren C.union (M.unionWith union)

unions :: Ord n => [MultiTrie n v c] -> MultiTrie n v c
unions = L.foldl union empty

-- TODO
intersection :: Ord n => MultiTrie n v c -> MultiTrie n v c -> MultiTrie n v c
intersection mt = nullToEmpty . zipContentsAndChildren C.intersection (M.intersectionWith intersection) mt 

intersections :: (Ord n, Bounded n, Enum n, Eq v, Bounded v, Enum v, C.Intersectible v c, C.Topable v c) => [MultiTrie n v c] -> MultiTrie n v c
intersections = L.foldl intersection top 

zipContentsAndChildren :: Ord n => (c v -> c v -> c v) -> (MultiTrieMap n v c -> MultiTrieMap n v c -> MultiTrieMap n v c) -> MultiTrie n v c -> MultiTrie n v c -> MultiTrie n v c
zipContentsAndChildren f g (MultiTrie vs1 m1) (MultiTrie vs2 m2) = MultiTrie (f vs1 vs2) (g m1 m2) 

flatten :: (Ord n, C.Unitable v c, F.Foldable c) => MultiTrie n (MultiTrie n v c) c -> MultiTrie n v c
flatten (MultiTrie mts mtm) = F.foldr union empty mts `union` MultiTrie C.empty (M.map flatten mtm)

applyCartesian :: (Ord n, C.Unitable w c, F.Foldable c, C.Mapable v w c, C.Mapable (v -> w) (MultiTrie n w c) c) => MultiTrie n (v -> w) c -> MultiTrie n v c -> MultiTrie n w c
applyCartesian mtf mtx = flatten $ map (`map` mtx) mtf

applyUniting :: (Ord n, C.Unitable w c, Applicative c) => MultiTrie n (v -> w) c -> MultiTrie n v c -> MultiTrie n w c
applyUniting = applyZippingChildren (M.unionWith union)

applyIntersecting :: (Ord n, C.Intersectible w c, Applicative c) => MultiTrie n (v -> w) c -> MultiTrie n v c -> MultiTrie n w c
applyIntersecting = applyZippingChildren (M.intersectionWith intersection)

applyZippingChildren :: (Ord n, Applicative c) => (MultiTrieMap n w c -> MultiTrieMap n w c -> MultiTrieMap n w c) -> MultiTrie n (v -> w) c -> MultiTrie n v c -> MultiTrie n w c
applyZippingChildren op mtf@(MultiTrie fs fm) mtx@(MultiTrie xs xm) =
    MultiTrie
        (fs <*> xs)
        (op
            (M.map (applyZippingChildren op mtf) xm)
            (M.map ((flip $ applyZippingChildren op) mtx) fm))

bindCartesian :: (Ord n, F.Foldable c, C.Unitable w c, C.Mapable v (MultiTrie n w c) c) => MultiTrie n v c -> (v -> MultiTrie n w c) -> MultiTrie n w c
bindCartesian mt fmt = flatten $ map fmt mt

toMap :: (Ord n, C.Elementary c) => MultiTrie n v c -> M.Map [n] (c v)
toMap (MultiTrie vs m) = if C.null vs then childrenMap else M.insert [] vs childrenMap
    where
        childrenMap =
            M.unions $
            M.elems $
            M.mapWithKey (\n -> M.mapKeys (n:)) $
            M.map toMap m

fromList :: (Ord n, C.Insertable v c) => [([n], v)] -> MultiTrie n v c
fromList = L.foldr (uncurry insert) empty

fromMaybe :: C.Elementary c => Maybe (MultiTrie n v c) -> MultiTrie n v c
fromMaybe Nothing = empty
fromMaybe (Just mt) = mt

toMaybe :: C.Elementary c => MultiTrie n v c -> Maybe (MultiTrie n v c)
toMaybe mt = if null mt then Nothing else Just mt

nullToEmpty :: C.Elementary c => MultiTrie n v c -> MultiTrie n v c
nullToEmpty mt = if null mt then empty else mt

cleanupEmpties :: (Ord n, C.Elementary c) => MultiTrie n v c -> MultiTrie n v c
cleanupEmpties (MultiTrie vs m) = nullToEmpty $ MultiTrie vs (M.map cleanupEmpties m)

toTree :: (n -> t) -> (c v -> t) -> MultiTrie n v c -> T.Tree t
toTree f g (MultiTrie vs m) = T.Node (g vs) $ M.elems $ M.mapWithKey namedChildToTree m
    where
        namedChildToTree k mt = T.Node (f k) [toTree f g mt]

draw :: (Show n, Show (c v)) => MultiTrie n v c -> String
draw = T.drawTree . toTree show show
