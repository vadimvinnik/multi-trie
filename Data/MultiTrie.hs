module Data.MultiTrie where

import Prelude hiding (lookup, map, null, repeat)
import qualified Data.Map as M
import qualified Data.Tree as T
import Control.Applicative hiding (empty)

import Data.Allable
import Data.Container as C

type MultiTrieMap n v c = M.Map n (MultiTrie n v c) 

data MultiTrie n v c = MultiTrie
    {
        values :: c v,
        children :: MultiTrieMap n v c
    }
    deriving (Eq, Show)

empty :: Elementary v c => MultiTrie n v c
empty = MultiTrie C.empty M.empty

top :: (Ord n, Bounded n, Enum n, Topable v c) => MultiTrie n v
top = MultiTrie allValues (M.fromList $ zip allValues $ L.repeat top) 

singleton :: Elementary v c => v -> MultiTrie n v c
singleton x = MultiTrie (C.singleton x) M.empty

repeat :: (Ord n, Bounded n, Enum n) => v -> MultiTrie n v
repeat x = MultiTrie (L.repeat x) (M.fromList $ zip allValues $ L.repeat $ repeat x)

null :: MultiTrie n v -> Bool
null (MultiTrie vs m) = L.null vs && L.all null (M.elems m)

size :: MultiTrie n v -> Int
size (MultiTrie vs m) = L.length vs + L.sum (L.map size (M.elems m))

lookup :: Ord n => [n] -> MultiTrie n v -> MultiTrie n v
lookup [] mt = mt
lookup (n:ns) (MultiTrie _ m) = lookup' ns (M.lookup n m)
    where
        lookup' _ Nothing = empty
        lookup' ns' (Just mt) = lookup ns' mt

fetch :: Ord n => [n] -> MultiTrie n v -> [v]
fetch ns = values . lookup ns

update :: Ord n => [n] -> (MultiTrie n v -> MultiTrie n v) -> MultiTrie n v -> MultiTrie n v
update [] f mt = f mt
update (n:ns) f (MultiTrie vs m) = MultiTrie vs (M.alter (toMaybe . update ns f . fromMaybe) n m)

put :: v -> MultiTrie n v -> MultiTrie n v
put x (MultiTrie vs m) = MultiTrie (x : vs) m

insert :: Ord n => [n] -> v -> MultiTrie n v -> MultiTrie n v
insert ns v = update ns (put v)

replace :: Ord n => [n] -> MultiTrie n v -> MultiTrie n v -> MultiTrie n v
replace ns mt1 = update ns (const mt1)

delete :: Ord n => [n] -> MultiTrie n v -> MultiTrie n v
delete ns = replace ns empty

unite :: Ord n => [n] -> MultiTrie n v -> MultiTrie n v -> MultiTrie n v
unite ns mt1 = update ns (union mt1)

intersect :: (Ord n, Eq v) => [n] -> MultiTrie n v -> MultiTrie n v -> MultiTrie n v
intersect ns mt1 = update ns (intersection mt1)

map :: Ord n => (v -> w) -> MultiTrie n v -> MultiTrie n w
map f = mapList (L.map f)

mapWithName :: Ord n => ([n] -> v -> w) -> MultiTrie n v -> MultiTrie n w
mapWithName f = mapListWithName (L.map . f) 

mapAll :: Ord n => [v -> w] -> MultiTrie n v -> MultiTrie n w
mapAll fs  = mapList (fs <*>)

mapAllWithName :: Ord n => [[n] -> v -> w] -> MultiTrie n v -> MultiTrie n w
mapAllWithName fs = mapListWithName (\ns -> (L.map ($ns) fs <*>))

mapList :: Ord n => ([v] -> [w]) -> MultiTrie n v -> MultiTrie n w
mapList fl (MultiTrie vs vm) = MultiTrie (fl vs) (M.map (mapList fl) vm)

mapListWithName :: Ord n => ([n] -> [v] -> [w]) -> MultiTrie n v -> MultiTrie n w
mapListWithName fl (MultiTrie vs vm) = MultiTrie (fl [] vs) (M.mapWithKey (\n -> mapListWithName $ fl . (n:)) vm)

cartesianProduct :: Ord n => MultiTrie n v -> MultiTrie n w -> MultiTrie n (v, w)
cartesianProduct mtv = applyCartesian (map (,) mtv)

union :: Ord n => MultiTrie n v -> MultiTrie n v -> MultiTrie n v
union = setop (++) (M.unionWith union)

unions :: Ord n => [MultiTrie n v] -> MultiTrie n v
unions = L.foldl union empty

intersection :: (Ord n, Eq v) => MultiTrie n v -> MultiTrie n v -> MultiTrie n v
intersection mt = nullToEmpty . setop L.intersect (M.intersectionWith intersection) mt 

intersections :: (Ord n, Bounded n, Enum n, Eq v, Bounded v, Enum v) => [MultiTrie n v] -> MultiTrie n v
intersections = L.foldl intersection top 

setop :: Ord n => ([v] -> [v] -> [v]) -> (MultiTrieMap n v -> MultiTrieMap n v -> MultiTrieMap n v) -> MultiTrie n v -> MultiTrie n v -> MultiTrie n v
setop f op (MultiTrie vs1 m1) (MultiTrie vs2 m2) = MultiTrie (f vs1 vs2) (op m1 m2) 

flatten :: Ord n => MultiTrie n (MultiTrie n v) -> MultiTrie n v
flatten (MultiTrie mts mtm) = unions mts `union` MultiTrie [] (M.map flatten mtm) 

applyCartesian :: Ord n => MultiTrie n (v -> w) -> MultiTrie n v -> MultiTrie n w
applyCartesian mtf mtx = flatten $ map (`map` mtx) mtf

applyUniting :: Ord n => MultiTrie n (v -> w) -> MultiTrie n v -> MultiTrie n w
applyUniting = applyop (M.unionWith union)

applyIntersecting :: (Ord n, Eq v, Eq w) => MultiTrie n (v -> w) -> MultiTrie n v -> MultiTrie n w
applyIntersecting = applyop (M.intersectionWith intersection)

applyop :: Ord n => (MultiTrieMap n w -> MultiTrieMap n w -> MultiTrieMap n w) -> MultiTrie n (v -> w) -> MultiTrie n v -> MultiTrie n w
applyop op mtf@(MultiTrie fs fm) mtx@(MultiTrie xs xm) =
    MultiTrie
        (fs <*> xs)
        (op
            (M.map (applyop op mtf) xm)
            (M.map ((flip $ applyop op) mtx) fm))

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
fromList = L.foldr (uncurry insert) empty

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

draw :: (Show n, Show v) => MultiTrie n v -> String
draw = T.drawTree . toTree show show
