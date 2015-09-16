{-# LANGUAGE FlexibleContexts #-}

-- | A MultiTrie is a trie (i.e. a prefix tree) with each node containing a list of values considered as a multiset.
module Data.MultiTrie where

import Prelude hiding (lookup, map, null, repeat)
import qualified Data.Foldable as F
import qualified Data.Map as M
import qualified Data.Tree as T
import qualified Data.List as L
import Control.Applicative hiding (empty)

-- | A map of labels onto child nodes
type MultiTrieMap n v = M.Map n (MultiTrie n v) 

-- | A node object
data MultiTrie n v = MultiTrie
    {
        values :: [v],                  -- ^ multiset
        children :: MultiTrieMap n v    -- ^ child nodes
    }
    deriving (Eq, Show)

-- | Constant: an empty MultiTrie. A neutral element with respect to 'union'.
empty :: MultiTrie n v
empty = MultiTrie [] M.empty

-- | A MultiTrie containing just one value in its root node's multiset and no child nodes
singleton :: v -> MultiTrie n v
singleton x = MultiTrie [x] M.empty

-- | An infinite MultiTrie that has, in each node, the same multiset of values and the same children names
repeat :: Ord n => [n] -> [v] -> MultiTrie n v
repeat ns xs = MultiTrie xs (M.fromList $ zip ns $ L.repeat $ repeat ns xs)

-- | A MultiTrie that has all possible values and all possible chid names in each node.
-- A neutral element with respect to 'intersection'. An opposite to the 'empty' MultiTrie.
top :: (Ord n, Bounded n, Enum n, Bounded v, Enum v) => MultiTrie n v
top = repeat allValues $ L.cycle allValues

-- | Check whether a MultiTrie is empty
null :: MultiTrie n v -> Bool
null (MultiTrie vs m) = L.null vs && L.all null (M.elems m)

-- | A total number of values in all nodes
size :: MultiTrie n v -> Int
size (MultiTrie vs m) = L.length vs + L.sum (L.map size (M.elems m))

-- | Select a MultiTrie subnode identified by the given path ('empty' if there is no such path)
lookup :: Ord n => [n] -> MultiTrie n v -> MultiTrie n v
lookup [] mt = mt
lookup (n:ns) (MultiTrie _ m) = maybe empty (lookup ns) (M.lookup n m)

-- | A multiset of values from a subnode identified by the given path (empty list if there is no such path)
valuesByPath :: Ord n => [n] -> MultiTrie n v -> [v]
valuesByPath ns = values . lookup ns

-- | Perform the given transformation on a subnode identified by the given path
update :: Ord n => [n] -> (MultiTrie n v -> MultiTrie n v) -> MultiTrie n v -> MultiTrie n v
update [] f mt = f mt
update (n:ns) f (MultiTrie vs m) = MultiTrie vs (M.alter (toMaybe . update ns f . fromMaybe) n m)

-- | Add a new value to the root node's multiset of values
add :: v -> MultiTrie n v -> MultiTrie n v
add v (MultiTrie vs m) = MultiTrie (v:vs) m

-- | Add a new value to a multiset of values in a subnode identified by the given path
addByPath :: Ord n => [n] -> v -> MultiTrie n v -> MultiTrie n v
addByPath ns v = update ns (add v)

-- | Replace a subnode identified by the given path with a new given MultiTrie
replace :: Ord n => [n] -> MultiTrie n v -> MultiTrie n v -> MultiTrie n v
replace ns mt1 = update ns (const mt1)

-- | Delete a subnode identified by the given path
delete :: Ord n => [n] -> MultiTrie n v -> MultiTrie n v
delete ns = replace ns empty

-- | Replace a subnode identified by the given path with its 'union' against a given MultiTrie
unite :: Ord n => [n] -> MultiTrie n v -> MultiTrie n v -> MultiTrie n v
unite ns mt1 = update ns (union mt1)

-- | Replace a subnode identified by the given path with its 'intersection' against a given MultiTrie
intersect :: (Ord n, Eq v) => [n] -> MultiTrie n v -> MultiTrie n v -> MultiTrie n v
intersect ns mt1 = update ns (intersection mt1)

-- | Map a function over each node's multiset of values
map :: Ord n => (v -> w) -> MultiTrie n v -> MultiTrie n w
map f = mapContainers (L.map f)

mapWithName :: Ord n => ([n] -> v -> w) -> MultiTrie n v -> MultiTrie n w
mapWithName f = mapContainersWithName (L.map . f) 

mapAll :: Ord n => [v -> w] -> MultiTrie n v -> MultiTrie n w
mapAll fs  = mapContainers (fs <*>)

mapAllWithName :: Ord n => [[n] -> v -> w] -> MultiTrie n v -> MultiTrie n w
mapAllWithName fs = mapContainersWithName (\ns -> (L.map ($ns) fs <*>))

mapContainers :: Ord n => ([v] -> [w]) -> MultiTrie n v -> MultiTrie n w
mapContainers fl (MultiTrie vs vm) = MultiTrie (fl vs) (M.map (mapContainers fl) vm)

mapContainersWithName :: Ord n => ([n] -> [v] -> [w]) -> MultiTrie n v -> MultiTrie n w
mapContainersWithName fl (MultiTrie vs vm) = MultiTrie (fl [] vs) (M.mapWithKey (\n -> mapContainersWithName $ fl . (n:)) vm)

cartesianProduct :: Ord n => MultiTrie n v -> MultiTrie n w -> MultiTrie n (v, w)
cartesianProduct mtv = applyCartesian (map (,) mtv)

union :: Ord n => MultiTrie n v -> MultiTrie n v -> MultiTrie n v
union = zipContentsAndChildren (++) (M.unionWith union)

unions :: Ord n => [MultiTrie n v] -> MultiTrie n v
unions = L.foldl union empty

intersection :: (Ord n, Eq v) => MultiTrie n v -> MultiTrie n v -> MultiTrie n v
intersection mt = nullToEmpty . zipContentsAndChildren listAsMultiSetIntersection (M.intersectionWith intersection) mt 

intersections :: (Ord n, Bounded n, Enum n, Eq v, Bounded v, Enum v) => [MultiTrie n v] -> MultiTrie n v
intersections = L.foldl intersection top 

zipContentsAndChildren :: Ord n => ([v] -> [v] -> [v]) -> (MultiTrieMap n v -> MultiTrieMap n v -> MultiTrieMap n v) -> MultiTrie n v -> MultiTrie n v -> MultiTrie n v
zipContentsAndChildren f g (MultiTrie vs1 m1) (MultiTrie vs2 m2) = MultiTrie (f vs1 vs2) (g m1 m2) 

flatten :: Ord n => MultiTrie n (MultiTrie n v) -> MultiTrie n v
flatten (MultiTrie mts mtm) = F.foldr union empty mts `union` MultiTrie [] (M.map flatten mtm)

applyCartesian :: Ord n => MultiTrie n (v -> w) -> MultiTrie n v -> MultiTrie n w
applyCartesian mtf mtx = flatten $ map (`map` mtx) mtf

applyUniting :: Ord n => MultiTrie n (v -> w) -> MultiTrie n v -> MultiTrie n w
applyUniting = applyZippingChildren (M.unionWith union)

applyIntersecting :: (Ord n, Eq w) => MultiTrie n (v -> w) -> MultiTrie n v -> MultiTrie n w
applyIntersecting = applyZippingChildren (M.intersectionWith intersection)

applyZippingChildren :: Ord n => (MultiTrieMap n w -> MultiTrieMap n w -> MultiTrieMap n w) -> MultiTrie n (v -> w) -> MultiTrie n v -> MultiTrie n w
applyZippingChildren op mtf@(MultiTrie fs fm) mtx@(MultiTrie xs xm) =
    MultiTrie
        (fs <*> xs)
        (op
            (M.map (applyZippingChildren op mtf) xm)
            (M.map ((flip $ applyZippingChildren op) mtx) fm))

bindCartesian :: Ord n => MultiTrie n v -> (v -> MultiTrie n w) -> MultiTrie n w
bindCartesian mt fmt = flatten $ map fmt mt

toMap :: Ord n => MultiTrie n v -> M.Map [n] [v]
toMap (MultiTrie vs m) = if L.null vs then childrenMap else M.insert [] vs childrenMap
    where
        childrenMap =
            M.unions $
            M.elems $
            M.mapWithKey (\n -> M.mapKeys (n:)) $
            M.map toMap m

fromList :: Ord n => [([n], v)] -> MultiTrie n v
fromList = L.foldr (uncurry addByPath) empty

fromMaybe :: Maybe (MultiTrie n v) -> MultiTrie n v
fromMaybe Nothing = empty
fromMaybe (Just mt) = mt

toMaybe :: MultiTrie n v -> Maybe (MultiTrie n v)
toMaybe mt = if null mt then Nothing else Just mt

nullToEmpty :: MultiTrie n v -> MultiTrie n v
nullToEmpty mt = if null mt then empty else mt

cleanupEmpties :: Ord n => MultiTrie n v -> MultiTrie n v
cleanupEmpties (MultiTrie vs m) = nullToEmpty $ MultiTrie vs (M.map cleanupEmpties m)

toTree :: (n -> t) -> ([v] -> t) -> MultiTrie n v -> T.Tree t
toTree f g (MultiTrie vs m) = T.Node (g vs) $ M.elems $ M.mapWithKey namedChildToTree m
    where
        namedChildToTree k mt = T.Node (f k) [toTree f g mt]

draw :: (Show n, Show [v]) => MultiTrie n v -> String
draw = T.drawTree . toTree show show

listAsMultiSetIntersection :: Eq a => [a] -> [a] -> [a]
listAsMultiSetIntersection [] _ = []
listAsMultiSetIntersection (x:xs) ys = if x `L.elem` ys
    then x : listAsMultiSetIntersection xs (L.delete x ys)
    else listAsMultiSetIntersection xs ys

allValues :: (Bounded a, Enum a) => [a]
allValues = [minBound..]

