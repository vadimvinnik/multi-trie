module MultiTrie where

import qualified Data.Map.Lazy as M

data MultiTrie n v = MultiTrie
    {
        values :: [v],
        children :: M.Map n (MultiTrie n v)
    }
    deriving (Eq, Show)
    -- todo: redefine (==) and show
    
empty :: MultiTrie n v
empty = MultiTrie [] M.empty

singleton :: v -> MultiTrie n v
singleton x = MultiTrie [x] M.empty

toMap :: MultiTrie n v -> M.Map [n] [v]
toMap = undefined

fromList :: [([n], v)] -> MultiTrie n v
fromList = undefined

null :: MultiTrie n v -> Bool
null = undefined

size :: MultiTrie n v -> Int
size = undefined

lookup :: [n] -> MultiTrie n v -> MultiTrie n v
lookup = undefined

fetch :: [n] -> MultiTrie n v -> [v]
fetch = undefined

update :: (MultiTrie n v -> MultiTrie n v) -> [n] -> MultiTrie n v -> MultiTrie n v
update = undefined

put :: v -> MultiTrie n v -> MultiTrie n v
put x (MultiTrie vs c) = MultiTrie (x : vs) c

insert :: [n] -> v -> MultiTrie n v -> MultiTrie n v
insert = undefined

replace :: [n] -> MultiTrie n v -> MultiTrie n v -> MultiTrie n v
replace = undefined

superpose :: [n] -> MultiTrie n v -> MultiTrie n v -> MultiTrie n v
superpose = undefined

map :: (v -> w) -> MultiTrie n v -> MultiTrie n w
map = undefined

apply :: MultiTrie n (v -> w) -> MultiTrie n v -> MultiTrie n w
apply = undefined

union :: MultiTrie n v -> MultiTrie n v -> MultiTrie n v
union = undefined

bind :: MultiTrie n v -> (v -> MultiTrie n w) -> MultiTrie n w
bind = undefined

showTree :: (Show n, Show v) => MultiTrie n v -> String
showTree = undefined

showTreeWith :: (n -> [v] -> String) -> MultiTrie n v -> Bool -> String
showTreeWith = undefined
