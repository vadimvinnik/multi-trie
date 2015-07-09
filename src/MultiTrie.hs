module MultiTrie where

import Prelude hiding (lookup, map, null)
import qualified Data.Map as M
import qualified Data.List as L
import Control.Applicative hiding (empty)

type MultiTrieMap n v = M.Map n (MultiTrie n v) 

data MultiTrie n v = MultiTrie
    {
        values :: [v],
        children :: MultiTrieMap n v
    }
    deriving (Eq, Show)

empty :: MultiTrie n v
empty = MultiTrie [] M.empty

singleton :: v -> MultiTrie n v
singleton x = MultiTrie [x] M.empty

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

superpose :: Ord n => [n] -> MultiTrie n v -> MultiTrie n v -> MultiTrie n v
superpose ns mt1 = update ns (union mt1)

map :: (v -> w) -> MultiTrie n v -> MultiTrie n w
map f (MultiTrie vs m) = MultiTrie (L.map f vs) (M.map (map f) m)

applyUniting :: Ord n => MultiTrie n (v -> w) -> MultiTrie n v -> MultiTrie n w
applyUniting mtf@(MultiTrie fs fm) mtx@(MultiTrie xs xm) =
    MultiTrie
        (fs <*> xs)
        (M.unionWith union (M.map (mtf `applyUniting`) xm) (M.map (`applyUniting` mtx) fm))

applyDeepening :: Ord n => MultiTrie n (v -> w) -> MultiTrie n v -> MultiTrie n w
applyDeepening = undefined

union :: Ord n => MultiTrie n v -> MultiTrie n v -> MultiTrie n v
union = setop (M.unionWith union)

intersection :: Ord n => MultiTrie n v -> MultiTrie n v -> MultiTrie n v
intersection = setop (M.intersectionWith intersection) 

setop :: Ord n => (MultiTrieMap n v -> MultiTrieMap n v -> MultiTrieMap n v) -> MultiTrie n v -> MultiTrie n v -> MultiTrie n v
setop op (MultiTrie vs1 m1) (MultiTrie vs2 m2) = MultiTrie (vs1 ++ vs2) (op m1 m2) 

bind :: MultiTrie n v -> (v -> MultiTrie n w) -> MultiTrie n w
bind = undefined

toMap :: MultiTrie n v -> M.Map [n] [v]
toMap = undefined

fromList :: Ord n => [([n], v)] -> MultiTrie n v
fromList = L.foldr (uncurry insert) empty

fromMaybe :: Maybe (MultiTrie n v) -> MultiTrie n v
fromMaybe Nothing = empty
fromMaybe (Just mt) = mt

toMaybe :: MultiTrie n v -> Maybe (MultiTrie n v)
toMaybe mt = if null mt then Nothing else Just mt

showTree :: (Show n, Show v) => MultiTrie n v -> String
showTree = undefined

showTreeWith :: (n -> [v] -> String) -> MultiTrie n v -> Bool -> String
showTreeWith = undefined
