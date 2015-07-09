module MultiTrie where

import qualified Data.Map.Lazy as M

data MultiTrie n v = MultiTrie
    {
        values :: [v],
        children :: M.Map n v
    }
    deriving (Eq, Show)
    -- todo: redefine (==) and show

empty :: MultiTrie n v
empty = MultiTrie [] M.empty

singleton :: v -> MultiTrie n v
singleton x = MultiTrie [x] M.empty

insertValue :: v -> MultiTrie n v -> MultiTrie n v
insertValue x (MultiTrie vs c) = MultiTrie (x : vs) c 