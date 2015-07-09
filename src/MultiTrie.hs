module MultiTrie where

import qualified Data.Map.Lazy as M

data MultiTrie n v = MultiTrie
    {
        values :: [v],
        children :: M.Map n v
    }

empty :: MultiTrie n v
empty = MultiTrie
    {
        values = [],
        children = M.empty
    }

singleton :: v -> MultiTrie n v
singleton x = MultiTrie
    {
        values = [x],
        children = M.empty
    }
