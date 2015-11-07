{-# LANGUAGE FlexibleContexts #-}

{- |
A multi-trie is a trie (i.e. a tree whose child nodes have distinct labels)
with each node containing a list of values considered as a multiset.

Lookup of a sub-trie under a certain path is defined in an obvious way.

The simplest possible multi-trie is 'empty' that has an empty multiset of
values and an no children.

Since the only essential feature of a multi-trie is carrying values, the
'empty' multi-trie could be equated with an absense of a multi-trie. In
particular, instead of saying that there is no sub-trie under path @p@ in a
multi-trie @t@, let us say that @t@ contains a path @p@, and it points to an
'empty' node. 

Therefore, every multi-trie could be considered as infinite, where each node
has children under all possible names - and some of nodes are 'empty'.

Some operations could be defined for multi-tries in a rather natural way,
including 'map', 'union', 'intersection', 'cartesianProduct'.

Moreover, a multi-trie can contain not only ordinary values but also functions
that makes it possible to apply a multi-trie of functions to a multi-trie of
argument values, combining results with 'cartesianProduct', 'union' or
'intersection'. This makes multi-tries a kind of 'Applicative' and 'Monad'.
-}

module Data.MultiTrie(
    -- * Type
    MultiTrie,
    -- * Simple constructors
    empty,
    singleton,
    leaf,
    repeat,
    top,
    add,
    -- * Simple selectors
    values,
    children,
    null,
    size,
    -- * Subnode access
    lookup,
    valuesByPath,
    update,
    addByPath,
    replace,
    delete,
    unite,
    intersect,
    -- * Mappings
    mtmap,
    mapWithPath,
    mapAll,
    mapAllWithPath,
    mapContainers,
    mapContainersWithPath,
    -- * High-level operations
    cartesianProduct,
    union,
    unions,
    unions1,
    intersection,
    intersections,
    intersections1,
    flatten,
    -- * Applications
    applyCartesian,
    applyUniting,
    applyIntersecting,
    bindCartesian,
    -- * Conversions
    toMap,
    toList,
    fromList,
    fromMaybe,
    toMaybe,
    -- * Debug
    draw
) where

import Prelude hiding (lookup, null, repeat)
import qualified Data.Foldable as F
import qualified Data.Map as M
import qualified Data.Tree as T
import qualified Data.List as L
import Control.Applicative hiding (empty)

-- | A map of labels onto child nodes.
type MultiTrieMap n v = M.Map n (MultiTrie n v) 

-- | A node object.
data MultiTrie n v = MultiTrie
    {
        values :: [v],                  -- ^ multiset
        children :: MultiTrieMap n v    -- ^ child nodes
    }
    deriving (Show)

instance Ord n => Functor (MultiTrie n) where
    fmap = mtmap

instance Ord n => Applicative (MultiTrie n) where
    pure = singleton
    (<*>) = applyCartesian

instance Ord n => Monad (MultiTrie n) where
    return = singleton
    (>>=) = bindCartesian

-- | Constant: an empty multi-trie. A neutral element with respect to 'union'.
empty :: MultiTrie n v
empty = MultiTrie [] M.empty

-- | A multi-trie containing just one value in its root and no child nodes.
singleton :: v -> MultiTrie n v
singleton x = leaf [x]

-- | A multi-trie containing the given multiset in its root and no child nodes.
leaf :: [v] -> MultiTrie n v
leaf vs = MultiTrie vs M.empty

{- |
An infinite multi-trie that has in each node the same multiset of values and,
under each name from the given set, a child identical to the root.
-}
repeat :: Ord n => [n] -> [v] -> MultiTrie n v
repeat ns xs =
    if   L.null xs
    then empty
    else MultiTrie xs (M.fromList $ zip ns $ L.repeat $ repeat ns xs)

{- |
A multi-trie that has all possible values and all possible chid names in each
node. A neutral element with respect to 'intersection'. An opposite to the
'empty' multi-trie.
-}
top :: (Ord n, Bounded n, Enum n, Bounded v, Enum v) => MultiTrie n v
top = repeat allValues $ L.cycle allValues

-- | Check whether a multi-trie is empty.
null :: MultiTrie n v -> Bool
null (MultiTrie vs m) = L.null vs && L.all null (M.elems m)

-- | A total number of values in all nodes.
size :: MultiTrie n v -> Int
size (MultiTrie vs m) = L.length vs + L.sum (L.map size (M.elems m))

{- TODO minViewWithKey and equivalence
isEqual :: (Ord n, Eq v) => MultiTrie n v -> MultiTrie n v -> Bool
isEqual (MultiTrie vs1 m1) (MultiTrie vs2 m2) = vs1 == vs2 && m1 == m2
-}

{-
Select a multi-trie subnode identified by the given path, or 'empty' if there
is no such path.
-}
lookup :: Ord n => [n] -> MultiTrie n v -> MultiTrie n v
lookup [] mt = mt
lookup (n:ns) (MultiTrie _ m) = maybe empty (lookup ns) (M.lookup n m)

{-
A multiset of values from a subnode identified by the given path (empty list
if there is no such path).
-}
valuesByPath :: Ord n => [n] -> MultiTrie n v -> [v]
valuesByPath ns = values . lookup ns

-- | Perform the given transformation on a subnode identified by the path.
update :: Ord n =>
    [n] ->
    (MultiTrie n v -> MultiTrie n v) ->
    MultiTrie n v ->
    MultiTrie n v
update [] f mt = f mt
update (n:ns) f (MultiTrie vs m) =
    MultiTrie vs (M.alter (toMaybe . update ns f . fromMaybe) n m)

-- | Add a new value to the root node's multiset of values.
add :: v -> MultiTrie n v -> MultiTrie n v
add v (MultiTrie vs m) = MultiTrie (v:vs) m

-- | Add a value to a multiset of values in a subnode identified by the path.
addByPath :: Ord n => [n] -> v -> MultiTrie n v -> MultiTrie n v
addByPath ns v = update ns (add v)

-- | Replace a subnode identified by the path with a new multi-trie.
replace :: Ord n => [n] -> MultiTrie n v -> MultiTrie n v -> MultiTrie n v
replace ns mt1 = update ns (const mt1)

-- | Delete a subnode identified by the given path.
delete :: Ord n => [n] -> MultiTrie n v -> MultiTrie n v
delete ns = replace ns empty

-- | Unite a subnode identified by the path with another multi-trie.
unite :: Ord n =>
    [n] ->
    MultiTrie n v ->
    MultiTrie n v ->
    MultiTrie n v
unite ns mt1 = update ns (union mt1)

-- | Intersect a subnode identified by the path with another multi-trie.
intersect :: (Ord n, Eq v) =>
    [n] ->
    MultiTrie n v ->
    MultiTrie n v ->
    MultiTrie n v
intersect ns mt1 = update ns (intersection mt1)

-- | Map a function over all values in a multi-trie.
mtmap :: Ord n => (v -> w) -> MultiTrie n v -> MultiTrie n w
mtmap f = mapContainers (L.map f)

-- | Map a function over all values, passing node paths as well.
mapWithPath :: Ord n => ([n] -> v -> w) -> MultiTrie n v -> MultiTrie n w
mapWithPath f = mapContainersWithPath (L.map . f) 

{-
Apply a multiset @F@ of functions to all values in a multi-trie. If @V@ is a
multi-set of values under a certain path @s@ in a multi-trie @P@, the result
@Q@ will contain under @s@ a multi-set of all @(f v)@ values, for all @v@ from
@V@ and all @f@ from F.
-}
mapAll :: Ord n => [v -> w] -> MultiTrie n v -> MultiTrie n w
mapAll fs  = mapContainers (fs <*>)

-- | Apply a multiset of functions to each value and its path.
mapAllWithPath :: Ord n => [[n] -> v -> w] -> MultiTrie n v -> MultiTrie n w
mapAllWithPath fs = mapContainersWithPath (\ns -> (L.map ($ns) fs <*>))

-- | Map a function over entire multisets contained in nodes.
mapContainers :: Ord n => ([v] -> [w]) -> MultiTrie n v -> MultiTrie n w
mapContainers fl (MultiTrie vs vm) =
    MultiTrie (fl vs) (M.mapMaybe (toMaybe . mapContainers fl) vm)

-- | Map a function over entire multisets, passing node path as well.
mapContainersWithPath :: Ord n =>
    ([n] -> [v] -> [w]) ->
    MultiTrie n v ->
    MultiTrie n w
mapContainersWithPath fl (MultiTrie vs vm) =
    MultiTrie
        (fl [] vs)
        (M.mapMaybeWithKey transformChild vm) where
    transformChild n = toMaybe . (mapContainersWithPath $ fl . (n:))

{- |
Cartesian product of two multi-tries @P@ and @Q@ is a multi-trie @R@ whose
paths are concatenations of any path @s@ from @P@ and every path @t@ from @Q@,
and values under @st@ in @R@ are pairs @(v, w)@ where @v@ is a value under @s@
in @P@, and @w@ is a value under @t@ in @Q@.
-}
cartesianProduct :: Ord n =>
    MultiTrie n v ->
    MultiTrie n w ->
    MultiTrie n (v, w)
cartesianProduct mtv = applyCartesian (mtmap (,) mtv)

{- |
Union of multi-tries @P@ and @Q@ is such a multi-trie @R@ that, under any path
@p@, @R@ contains a union of a multiset that is contained in @P@ under @p@ and
a multiset contained in @Q@ under @p@.
-}
union :: Ord n => MultiTrie n v -> MultiTrie n v -> MultiTrie n v
union = zipContentsAndChildren (++) (M.unionWith union)

-- | Union of a list of multi-tries.
unions :: Ord n => [MultiTrie n v] -> MultiTrie n v
unions = L.foldl union empty

-- | Union of a non-empty list of multi-tries.
unions1 :: Ord n => [MultiTrie n v] -> MultiTrie n v
unions1 = L.foldl1 union

{- |
Intersection of multi-tries @P@ and @Q@ is such a multi-trie @R@ that, under
any path @p@, @R@ contains a union of a multiset that is contained in @P@ under
@p@ and a multiset contained in @Q@ under @p@.
-}
intersection :: (Ord n, Eq v) =>
    MultiTrie n v ->
    MultiTrie n v ->
    MultiTrie n v
intersection mt = nullToEmpty .
    zipContentsAndChildren
        listAsMultiSetIntersection
        (\x y -> M.filter (not . null) (M.intersectionWith intersection x y))
        mt 

-- | Intersection of a non-empty list of multi-tries.
intersections :: (Ord n, Bounded n, Enum n, Eq v, Bounded v, Enum v) =>
    [MultiTrie n v] ->
    MultiTrie n v
intersections = L.foldl intersection top 

-- | Intersection of a non-empty list of multi-tries.
intersections1 :: (Ord n, Eq v) =>
    [MultiTrie n v] ->
    MultiTrie n v
intersections1 = L.foldl1 intersection

{- |
Given a multi-trie whose values are multi-tries in their turn, convert it into
a 'plain' multi-trie.  If @P@ is a multi-trie that contains a multi-trie @Q@ as
its value under a path @s@, and @Q@ contains a value @x@ under a path @t@, then
the plain multi-trie @R@ will contain @x@ as a value under a path @st@ (that is
a concatenation of @s@ and @t@).
-}
flatten :: Ord n => MultiTrie n (MultiTrie n v) -> MultiTrie n v
flatten (MultiTrie mts mtm) =
    F.foldr union empty mts `union` MultiTrie [] (M.map flatten mtm)

{- |
Given a multi-trie @P@ of functions and a multi-trie @Q@ of values, apply each
function from @P@ to each value from @Q@ arranging results as described in
'cartesianProduct'.
-}
applyCartesian :: Ord n =>
    MultiTrie n (v -> w) ->
    MultiTrie n v ->
    MultiTrie n w
applyCartesian mtf mtx = flatten $ mtmap (`mtmap` mtx) mtf

{- |
Given a multi-trie @P@ of functions and a multi-trie @Q@ of values, apply each
function from @P@ to each value from @Q@ combining results with 'union'.
-}
applyUniting :: Ord n => MultiTrie n (v -> w) -> MultiTrie n v -> MultiTrie n w
applyUniting = applyZippingChildren (M.unionWith union)

{- |
Given a multi-trie @P@ of functions and a multi-trie @Q@ of values, apply each
function from @P@ to each value from @Q@ combining results with 'intersection'.
-}
applyIntersecting :: (Ord n, Eq w) =>
    MultiTrie n (v -> w) ->
    MultiTrie n v ->
    MultiTrie n w
applyIntersecting = applyZippingChildren (M.intersectionWith intersection)

{-
Given a multi-tree @P@ of values and a function @f@ that maps an arbitrary value
to a multi-tree, apply the function @f@ to each value from @P@ and combine the
results as described in 'flatten'.
-}
bindCartesian :: Ord n => MultiTrie n v -> (v -> MultiTrie n w) -> MultiTrie n w
bindCartesian mt fmt = flatten $ mtmap fmt mt

{-
Convert a multi-trie @P@ to a map @M@ whose keys are paths from @P@, and values
in @M@ are respective lists representing multi-sets of values in @P@.
-}
toMap :: Ord n => MultiTrie n v -> M.Map [n] [v]
toMap (MultiTrie vs m) = if L.null vs
        then childrenMap
        else M.insert [] vs childrenMap
    where
        childrenMap =
            M.unions $
            M.elems $
            M.mapWithKey (\n -> M.mapKeys (n:)) $
            M.map toMap m

-- | Convert a multi-trie to a list of path-value pairs.
toList :: Ord n => MultiTrie n v -> [([n], v)]
toList (MultiTrie vs m) = (map ((,) []) vs) ++
    (
        L.concat $
        L.map (\(n, ps) -> L.map (\(ns, ws) -> (n:ns, ws)) ps) $
        M.toList $
        M.map toList m
    )

-- | Convert a list of path-value pairs to a multi-trie.
fromList :: Ord n => [([n], v)] -> MultiTrie n v
fromList = L.foldr (uncurry addByPath) empty

-- | Map 'Nothing' to 'empty' and @Just mt@ to @mt@.
fromMaybe :: Maybe (MultiTrie n v) -> MultiTrie n v
fromMaybe = maybe empty id

-- | Map 'empty' to 'Nothing' and a non-empty @mt@ to @Just mt@.
toMaybe :: MultiTrie n v -> Maybe (MultiTrie n v)
toMaybe mt = if null mt then Nothing else Just mt

-- | Convert a multi-trie into an ASCII-drawn tree.
draw :: (Show n, Show [v]) => MultiTrie n v -> String
draw = T.drawTree . toTree show show

--
-- Internal helper functions
--

nullToEmpty :: MultiTrie n v -> MultiTrie n v
nullToEmpty mt = if null mt then empty else mt

zipContentsAndChildren :: Ord n =>
    ([v] -> [v] -> [v]) ->
    (MultiTrieMap n v -> MultiTrieMap n v -> MultiTrieMap n v) ->
    MultiTrie n v ->
    MultiTrie n v ->
    MultiTrie n v
zipContentsAndChildren f g (MultiTrie vs1 m1) (MultiTrie vs2 m2) =
    MultiTrie (f vs1 vs2) (g m1 m2) 

applyZippingChildren :: Ord n =>
    (MultiTrieMap n w -> MultiTrieMap n w -> MultiTrieMap n w) ->
    MultiTrie n (v -> w) ->
    MultiTrie n v ->
    MultiTrie n w
applyZippingChildren op mtf@(MultiTrie fs fm) mtx@(MultiTrie xs xm) =
    MultiTrie
        (fs <*> xs)
        (op
            (M.map (applyZippingChildren op mtf) xm)
            (M.map ((flip $ applyZippingChildren op) mtx) fm))

toTree :: (n -> t) -> ([v] -> t) -> MultiTrie n v -> T.Tree t
toTree f g (MultiTrie vs m) =
    T.Node (g vs) $ M.elems $ M.mapWithKey namedChildToTree m
    where
        namedChildToTree k mt = T.Node (f k) [toTree f g mt]

listAsMultiSetIntersection :: Eq a => [a] -> [a] -> [a]
listAsMultiSetIntersection [] _ = []
listAsMultiSetIntersection (x:xs) ys = if x `L.elem` ys
    then x : listAsMultiSetIntersection xs (L.delete x ys)
    else listAsMultiSetIntersection xs ys

allValues :: (Bounded a, Enum a) => [a]
allValues = [minBound..]

