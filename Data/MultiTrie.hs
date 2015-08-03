module Data.MultiTrie where

import Prelude hiding (lookup, map, null, repeat)
import qualified Data.Map as M
import qualified Data.Tree as T
import qualified Data.List as L
import Control.Applicative hiding (empty)

import Data.Allable
import qualified Data.Container as C

type MultiTrieMap n v c = M.Map n (MultiTrie n v c) 

data MultiTrie n v c = MultiTrie
    {
        values :: c v,
        children :: MultiTrieMap n v c
    }
    deriving (Eq, Show)

empty :: C.Elementary v c => MultiTrie n v c
empty = MultiTrie C.empty M.empty

top :: (Ord n, Bounded n, Enum n, C.Topable v c) => MultiTrie n v c
top = MultiTrie C.top (M.fromList $ zip allValues $ L.repeat top) 

singleton :: C.Elementary v c => v -> MultiTrie n v c
singleton x = MultiTrie (C.singleton x) M.empty

repeat :: (Ord n, Bounded n, Enum n, C.Repeatable v c) => v -> MultiTrie n v c
repeat x = MultiTrie (C.repeat x) (M.fromList $ zip allValues $ L.repeat $ repeat x)

null :: C.Elementary v c => MultiTrie n v c -> Bool
null (MultiTrie vs m) = C.null vs && L.all null (M.elems m)

size :: C.Elementary v c => MultiTrie n v c -> Int
size (MultiTrie vs m) = C.count vs + L.sum (L.map size (M.elems m))

lookup :: Ord n => [n] -> MultiTrie n v c -> MultiTrie n v c
lookup [] mt = mt
lookup (n:ns) (MultiTrie _ m) = lookup' ns (M.lookup n m)
    where
        lookup' _ Nothing = empty
        lookup' ns' (Just mt) = lookup ns' mt

fetch :: Ord n => [n] -> MultiTrie n v c -> c v
fetch ns = values . lookup ns

update :: Ord n => [n] -> (MultiTrie n v c -> MultiTrie n v c) -> MultiTrie n v c -> MultiTrie n v c
update [] f mt = f mt
update (n:ns) f (MultiTrie vs m) = MultiTrie vs (M.alter (toMaybe . update ns f . fromMaybe) n m)

put :: v -> MultiTrie n v c -> MultiTrie n v c
put x (MultiTrie vs m) = MultiTrie (C.insert x vs) m

insert :: (Ord n, C.Insertable v c) => [n] -> v -> MultiTrie n v c -> MultiTrie n v c
insert ns v = update ns (put v)

replace :: Ord n => [n] -> MultiTrie n v c -> MultiTrie n v c -> MultiTrie n v c
replace ns mt1 = update ns (const mt1)

delete :: Ord n => [n] -> MultiTrie n v c -> MultiTrie n v c
delete ns = replace ns empty

unite :: (Ord n, C.Unitable v c) => [n] -> MultiTrie n v c -> MultiTrie n v c -> MultiTrie n v c
unite ns mt1 = update ns (union mt1)

intersect :: (Ord n, C.Intersectible v c) => [n] -> MultiTrie n v c -> MultiTrie n v c -> MultiTrie n v c
intersect ns mt1 = update ns (intersection mt1)

map :: (Ord n, C.Elementary v c) => (v -> w) -> MultiTrie n v c -> MultiTrie n w c
map f = mapList (C.map f)

mapWithName :: (Ord n, C.Elementary v c) => ([n] -> v -> w) -> MultiTrie n v c -> MultiTrie n w c
mapWithName f = mapListWithName (C.map . f) 

mapAll :: Ord n => [v -> w] -> MultiTrie n v c -> MultiTrie n w c
mapAll fs  = mapList (fs <*>)

mapAllWithName :: Ord n => [[n] -> v -> w] -> MultiTrie n v c -> MultiTrie n w c
mapAllWithName fs = mapListWithName (\ns -> (L.map ($ns) fs <*>))

mapList :: Ord n => (c v -> c w) -> MultiTrie n v c -> MultiTrie n w c
mapList fl (MultiTrie vs vm) = MultiTrie (fl vs) (M.map (mapList fl) vm)

mapListWithName :: Ord n => ([n] -> c v -> c w) -> MultiTrie n v c -> MultiTrie n w c
mapListWithName fl (MultiTrie vs vm) = MultiTrie (fl [] vs) (M.mapWithKey (\n -> mapListWithName $ fl . (n:)) vm)

cartesianProduct :: Ord n => MultiTrie n v c -> MultiTrie n w c -> MultiTrie n (v, w) c
cartesianProduct mtv = applyCartesian (map (,) mtv)

union :: (Ord n, C.Unitable v c) => MultiTrie n v c -> MultiTrie n v c -> MultiTrie n v c
union = setop (++) (M.unionWith union)

unions :: Ord n => [MultiTrie n v c] -> MultiTrie n v c
unions = L.foldl union empty

intersection :: (Ord n, Eq v) => MultiTrie n v c -> MultiTrie n v c -> MultiTrie n v c
intersection mt = nullToEmpty . setop L.intersect (M.intersectionWith intersection) mt 

intersections :: (Ord n, Bounded n, Enum n, Eq v, Bounded v, Enum v) => [MultiTrie n v c] -> MultiTrie n v c
intersections = L.foldl intersection top 

setop :: Ord n => ([v] -> [v] -> [v]) -> (MultiTrieMap n v c -> MultiTrieMap n v c -> MultiTrieMap n v c) -> MultiTrie n v c -> MultiTrie n v c -> MultiTrie n v c
setop f op (MultiTrie vs1 m1) (MultiTrie vs2 m2) = MultiTrie (f vs1 vs2) (op m1 m2) 

flatten :: Ord n => MultiTrie n (MultiTrie n v c) c -> MultiTrie n v c
flatten (MultiTrie mts mtm) = unions mts `union` MultiTrie [] (M.map flatten mtm) 

applyCartesian :: Ord n => MultiTrie n (v -> w) c -> MultiTrie n v c -> MultiTrie n w c
applyCartesian mtf mtx = flatten $ map (`map` mtx) mtf

applyUniting :: Ord n => MultiTrie n (v -> w) c -> MultiTrie n v c -> MultiTrie n w c
applyUniting = applyop (M.unionWith union)

applyIntersecting :: (Ord n, Eq v, Eq w) => MultiTrie n (v -> w) c -> MultiTrie n v c -> MultiTrie n w c
applyIntersecting = applyop (M.intersectionWith intersection)

applyop :: Ord n => (MultiTrieMap n w c -> MultiTrieMap n w c -> MultiTrieMap n w c) -> MultiTrie n (v -> w) c -> MultiTrie n v c -> MultiTrie n w c
applyop op mtf@(MultiTrie fs fm) mtx@(MultiTrie xs xm) =
    MultiTrie
        (fs <*> xs)
        (op
            (M.map (applyop op mtf) xm)
            (M.map ((flip $ applyop op) mtx) fm))

bindCartesian :: Ord n => MultiTrie n v c -> (v -> MultiTrie n w c) -> MultiTrie n w c
bindCartesian mt fmt = flatten $ map fmt mt

toMap :: Ord n => MultiTrie n v c -> M.Map [n] [v]
toMap (MultiTrie vs m) = if L.null vs then childrenMap else M.insert [] vs childrenMap
    where
        childrenMap =
            M.unions $
            M.elems $
            M.mapWithKey (\n -> M.mapKeys (n:)) $
            M.map toMap m

fromList :: Ord n => [([n], v)] -> MultiTrie n v c
fromList = L.foldr (uncurry insert) empty

fromMaybe :: Maybe (MultiTrie n v c) -> MultiTrie n v c
fromMaybe Nothing = empty
fromMaybe (Just mt) = mt

toMaybe :: MultiTrie n v c -> Maybe (MultiTrie n v c)
toMaybe mt = if null mt then Nothing else Just mt

nullToEmpty :: MultiTrie n v c -> MultiTrie n v c
nullToEmpty mt = if null mt then empty else mt

cleanupEmpties :: Ord n => MultiTrie n v c -> MultiTrie n v c
cleanupEmpties (MultiTrie vs m) = nullToEmpty $ MultiTrie vs (M.map cleanupEmpties m)

toTree :: (n -> t) -> ([v] -> t) -> MultiTrie n v c -> T.Tree t
toTree f g (MultiTrie vs m) = T.Node (g vs) $ M.elems $ M.mapWithKey namedChildToTree m
    where
        namedChildToTree k mt = T.Node (f k) [toTree f g mt]

draw :: (Show n, Show v) => MultiTrie n v c -> String
draw = T.drawTree . toTree show show
