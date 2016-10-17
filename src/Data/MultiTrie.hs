--
-- Vadim Vinnik, 2015-16
-- vadim.vinnik@gmail.com
--

{-# LANGUAGE FlexibleContexts #-}

{- |
A 'MultiTrie' @v d@ is a trie (i.e. a tree whose child nodes have distinct
labels, or atomic names, of type @v@) with each node containing a list of values
of type @d@ that could be considered as a set or a multiset.  It represents a
multivalued naming with compound names: each path, or a compound name (i.e. a
chain of labels) has a (possibly empty) list of values.

The simplest possible 'MultiTrie' is 'empty' that has an empty list of values
and no child nodes.  Since the only essential feature of a 'MultiTrie' is
carrying values, the 'empty' 'MultiTrie' could be equated with an absense of a
'MultiTrie'.  In particular, instead of saying that there is no sub-trie under
some path in a 'MultiTrie', let us say that the path points to an 'empty' node.
Therefore, every 'MultiTrie' could be considered as infinite, having child nodes
under all possible names - and some of the nodes are 'empty'.

Some operations could be defined for 'MultiTrie's in a natural way, including
'filter', 'union', 'intersection', 'cartesian'.  Obviously, 'empty' is a neutral
element of 'union'.  Cartesian product is 'empty' if any of the two operands is
'empty'.

A unary function @f@ can be applied to each value in each node of a 'MultiTrie'
that results in a 'map' function.  Moreover, a 'MultiTrie' can contain not only
ordinary values but also functions that makes it possible to apply a 'MultiTrie'
of functions to a 'MultiTrie' of argument values, combining results with
'cartesian'.  A 'MultiTrie' whose values are, in their turn,  'MultiTrie's, can
be 'flatten'ed.  This makes 'MultiTrie's an instance of 'Functor', Applicative'
and 'Monad' classes.

For a detailed description of the multivalued naming with compound names as a a
mathematical notion, its operations and properties, see an article distributed
with this package as a LaTeX source.
-}

module Data.MultiTrie(
    -- * Type
    MultiTrie,
    -- * Simple constructors
    empty,
    singleton,
    leaf,
    repeat,
    updateValues,
    addValue,
    -- * Simple selectors
    values,
    children,
    null,
    size,
    -- * Comparison
    areEqualStrict,
    areEqualWeak,
    areEquivalentUpTo,
    -- * Subnode access
    subnode,
    subnodeUpdate,
    subnodeAddValue,
    subnodeReplace,
    subnodeDelete,
    subnodeUnite,
    subnodeIntersect,
    -- * Filtration
    filter,
    project,
    filterOnNames,
    filterWithNames,
    -- * Mappings
    map,
    mapWithName,
    mapMany,
    mapManyWithName,
    mapOnLists,
    mapOnListsWithName,
    -- * High-level operations
    cartesian,
    union,
    unions,
    intersection,
    intersections1,
    flatten,
    -- * Applications
    apply,
    bind,
    -- * Conversions
    toMap,
    toList,
    fromList,
    fromMaybe,
    toMaybe,
    -- * Debug
    draw,
    -- * Other
    listAsMultiSetEquals,
    areMapsEquivalentUpTo 
) where

import Prelude hiding (null, repeat, map, filter)
import qualified Data.Foldable as F
import qualified Data.Map as M
import qualified Data.Tree as T
import qualified Data.List as L
import Control.Applicative hiding (empty)
import Data.Composition((.:))

-- | A map of atomic names onto child nodes.
type MultiTrieMap v d = M.Map v (MultiTrie v d) 

-- | A trie consists of a list of values and labelled child tries.
data MultiTrie v d = MultiTrie
    {
        values :: [d],
        children :: MultiTrieMap v d
    }
    deriving (Show)

instance Ord v => Functor (MultiTrie v) where
    fmap = map

instance Ord v => Applicative (MultiTrie v) where
    pure = singleton
    (<*>) = apply

instance Ord v => Monad (MultiTrie v) where
    return = singleton
    (>>=) = bind

instance (Ord v, Eq d) => Eq (MultiTrie v d) where
    (==) = areEqualStrict

-- | An empty 'MultiTrie' constant. A neutral element of 'union' and zero of
-- 'cartesian'.
empty :: MultiTrie v d
empty = MultiTrie [] M.empty

-- | A 'MultiTrie' containing just one value in its root and no child nodes.
singleton :: d -> MultiTrie v d
singleton d = leaf [d]

-- | A 'MultiTrie' containing the given list in its root and no child nodes.
leaf ::
    [d] ->
    MultiTrie v d
leaf ds = MultiTrie ds M.empty

-- | An infinite 'MultiTrie' that has in each node the same list of values and,
-- under each name from the given set, a child identical to the root.
repeat :: Ord v =>
    [v] ->
    [d] ->
    MultiTrie v d
repeat vs ds =
    if   L.null ds
    then empty
    else MultiTrie ds (M.fromList $ zip vs $ L.repeat $ repeat vs ds)

-- | Change a list in the root node with a function and leave children intact.
updateValues ::
    ([d] -> [d]) ->
    MultiTrie v d ->
    MultiTrie v d
updateValues f (MultiTrie ds m) = MultiTrie (f ds) m

-- | Add a new value to the root node's list of values.
addValue ::
    d ->
    MultiTrie v d ->
    MultiTrie v d
addValue d = updateValues (d:)

-- | Check if a 'MultiTrie' is empty.
null ::
    MultiTrie v d ->
    Bool
null (MultiTrie ds m) = L.null ds && L.all null (M.elems m)

-- | A total number of values in all nodes.
size ::
    MultiTrie v d ->
    Int
size (MultiTrie ds m) = L.length ds + L.sum (L.map size (M.elems m))

-- | Check for equality counting the order of elements.
areEqualStrict :: (Ord v, Eq d) =>
    MultiTrie v d ->
    MultiTrie v d ->
    Bool
areEqualStrict = areEquivalentUpTo (==)

-- | Check for equality ignoring the order of elements.
areEqualWeak :: (Ord v, Eq d) =>
    MultiTrie v d ->
    MultiTrie v d ->
    Bool
areEqualWeak = areEquivalentUpTo listAsMultiSetEquals

-- | Check if two 'MultiTrie's, @t1@ and @t2@, are equivalent up to a custom
-- list equivalence predicate @p@.  True if and only if (1) both 'MultiTrie's
-- have non-empty nodes at the same paths and (2) for each such path @w@, value
-- lists from @t1@ and @t2@ under @w@ are equivalent, i.e. satisfy @p@.
areEquivalentUpTo :: (Ord v, Eq d) =>
    ([d] -> [d] -> Bool) ->
    MultiTrie v d ->
    MultiTrie v d ->
    Bool
areEquivalentUpTo p (MultiTrie ds1 m1) (MultiTrie ds2 m2) =
    (p ds1 ds2) &&
    (areMapsEquivalentUpTo (areEquivalentUpTo p) m1 m2)

-- | Select a 'MultiTrie' subnode identified by the given path, or 'empty' if
-- there is no such path.
subnode :: Ord v =>
    [v] ->
    MultiTrie v d ->
    MultiTrie v d
subnode [] t = t
subnode (v:vs) (MultiTrie _ m) = maybe empty (subnode vs) (M.lookup v m)

-- | Perform the given transformation on a subnode identified by the path.
subnodeUpdate :: Ord v =>
    [v] ->
    (MultiTrie v d -> MultiTrie v d) ->
    MultiTrie v d ->
    MultiTrie v d
subnodeUpdate [] f t = f t
subnodeUpdate (v:vs) f (MultiTrie ds m) =
    MultiTrie ds (M.alter (toMaybe . subnodeUpdate vs f . fromMaybe) v m)

-- | Add a value to a list of values in a subnode identified by the path.
subnodeAddValue :: Ord v =>
    [v] ->
    d ->
    MultiTrie v d ->
    MultiTrie v d
subnodeAddValue vs d = subnodeUpdate vs (addValue d)

-- | Replace a subnode identified by the path with a new 'MultiTrie'.
subnodeReplace :: Ord v =>
    [v] ->
    MultiTrie v d ->
    MultiTrie v d ->
    MultiTrie v d
subnodeReplace vs t = subnodeUpdate vs (const t)

-- | Delete a subnode identified by the given path.
subnodeDelete :: Ord v =>
    [v] ->
    MultiTrie v d ->
    MultiTrie v d
subnodeDelete vs = subnodeReplace vs empty

-- | Unite a subnode identified by the path with another 'MultiTrie'.
subnodeUnite :: Ord v =>
    [v] ->
    MultiTrie v d ->
    MultiTrie v d ->
    MultiTrie v d
subnodeUnite vs t = subnodeUpdate vs (union t)

-- | Intersect a subnode identified by the path with another 'MultiTrie'.
subnodeIntersect :: (Ord v, Eq d) =>
    [v] ->
    MultiTrie v d ->
    MultiTrie v d ->
    MultiTrie v d
subnodeIntersect vs t = subnodeUpdate vs (intersection t)

-- | Leave only those values that satisfy the predicate @p@.
filter :: Ord v => (d -> Bool) -> MultiTrie v d -> MultiTrie v d
filter p = mapOnLists (L.filter p)

-- | Leave only the nodes whose compound names are in the given list.
project :: Ord v => [[v]] -> MultiTrie v d -> MultiTrie v d
project vss = filterOnNames ((flip L.elem) vss)

-- | Leave only those nodes whose compound names satisfy the predicate @p@.
filterOnNames :: Ord v => ([v] -> Bool) -> MultiTrie v d -> MultiTrie v d
filterOnNames p = filterWithNames (flip (const p))

-- | Leave only those values that, with their compound names, satisfy the
-- predicate @p@.
filterWithNames :: Ord v => ([v] -> d -> Bool) -> MultiTrie v d -> MultiTrie v d
filterWithNames p = mapOnListsWithName (\vs ds -> L.filter (p vs) ds)

-- | Map a function over all values in a 'MultiTrie'.
map :: Ord v =>
    (d1 -> d2) ->
    MultiTrie v d1 ->
    MultiTrie v d2
map f = mapOnLists (L.map f)

-- | Map a function over all values with their compound names.
mapWithName :: Ord v =>
    ([v] -> d1 -> d2) ->
    MultiTrie v d1 ->
    MultiTrie v d2
mapWithName f = mapOnListsWithName (L.map . f) 

-- | Apply a list of functions to all values in a 'MultiTrie'.
mapMany :: Ord v =>
    [d1 -> d2] ->
    MultiTrie v d1 ->
    MultiTrie v d2
mapMany fs  = mapOnLists (fs <*>)

-- | Apply a list of functions to each value and its compound name.
mapManyWithName :: Ord v =>
    [[v] -> d1 -> d2] ->
    MultiTrie v d1 ->
    MultiTrie v d2
mapManyWithName fs = mapOnListsWithName (\vs -> (L.map ($vs) fs <*>))

-- | Map a function over entire lists contained in nodes.
mapOnLists :: Ord v =>
    ([d1] -> [d2]) ->
    MultiTrie v d1 ->
    MultiTrie v d2
mapOnLists f (MultiTrie ds m) =
    MultiTrie (f ds) (M.mapMaybe (toMaybe . mapOnLists f) m)

-- | Map a function over entire lists in all nodes, with their compound names.
mapOnListsWithName :: Ord v =>
    ([v] -> [d1] -> [d2]) ->
    MultiTrie v d1 ->
    MultiTrie v d2
mapOnListsWithName f (MultiTrie ds m) =
    MultiTrie
        (f [] ds)
        (M.mapMaybeWithKey transformChild m)
    where
        transformChild v = toMaybe . (mapOnListsWithName $ f . (v:))

-- | Cartesian product of two 'MultiTrie's, @t1@ and @t2@. The resulting
-- 'MultiTrie' consists of all possible pairs @(x1, x2)@ under a concatenated
-- name @v1 ++ v2@ where @x1@ is a value in @t1@ under a name @v1@, and @x2@ is
-- a value from @t2@ under the name @v2@.
cartesian :: Ord v =>
    MultiTrie v d1 ->
    MultiTrie v d2 ->
    MultiTrie v (d1, d2)
cartesian t = apply (map (,) t)

-- | Union of 'MultiTrie's.
union :: Ord v =>
    MultiTrie v d ->
    MultiTrie v d ->
    MultiTrie v d
union = zipContentsAndChildren (++) (M.unionWith union)

-- | Union of a list of 'MultiTrie's.
unions :: Ord v =>
    [MultiTrie v d] ->
    MultiTrie v d
unions = L.foldl union empty

-- | Intersection of 'MultiTrie's.
intersection :: (Ord v, Eq d) =>
    MultiTrie v d ->
    MultiTrie v d ->
    MultiTrie v d
intersection = nullToEmpty .:
    zipContentsAndChildren
        listAsMultiSetIntersection
        ((M.filter (not . null)) .: (M.intersectionWith intersection))

-- | Intersection of a non-empty list of 'MultiTrie's.
intersections1 :: (Ord v, Eq d) =>
    [MultiTrie v d] ->
    MultiTrie v d
intersections1 = L.foldl1 intersection

-- | Flatten a 'MultiTrie' whose values are, in their turn, 'MultiTrie's.
flatten :: Ord v =>
    MultiTrie v (MultiTrie v d) ->
    MultiTrie v d
flatten (MultiTrie ts m) =
    F.foldr union empty ts `union` MultiTrie [] (M.map flatten m)

-- | Given a 'MultiTrie' @t1@ of functions and a 'MultiTrie' @t2@ of values, for
-- all compound names @v1@ and @v2@, apply each function named by @v1@ in @t1@
-- to each value named by @v2@ in @t2@ and put the result into a new 'MultiTrie'
-- under a name @v1 ++ v2@.
apply :: Ord v =>
    MultiTrie v (d1 -> d2) ->
    MultiTrie v d1 ->
    MultiTrie v d2
apply t1 t2 = flatten $ map ((flip map) t2) t1

-- | Given a 'MultiTrie' @t@ of values and a function @f@ that maps an arbitrary
-- value to a 'MultiTrie', apply the function @f@ to each value from @t@ and
-- 'flatten' the result.
bind :: Ord v =>
    MultiTrie v d1 ->
    (d1 -> MultiTrie v d2) ->
    MultiTrie v d2
bind = flatten .: (flip map)

-- | Convert a 'MultiTrie' @t@ to a `Data.Map` of compound names into value
-- lists.
toMap :: Ord v =>
    MultiTrie v d ->
    M.Map [v] [d]
toMap (MultiTrie ds m) = if L.null ds
        then childrenMap
        else M.insert [] ds childrenMap
    where
        childrenMap =
            M.unions $
            M.elems $
            M.mapWithKey (\v -> M.mapKeys (v:)) $
            M.map toMap m

-- | Convert a 'MultiTrie' to a list of path-value pairs.
toList :: Ord v =>
    MultiTrie v d ->
    [([v], d)]
toList (MultiTrie ds m) = (L.map ((,) []) ds) ++
    (
        L.concat $
        L.map (\(v, ps) -> L.map (\(vs, ds') -> (v:vs, ds')) ps) $
        M.toList $
        M.map toList m
    )

-- | Convert a list of path-value pairs to a 'MultiTrie'.
fromList :: Ord v =>
    [([v], d)] ->
    MultiTrie v d
fromList = L.foldr (uncurry subnodeAddValue) empty

-- | Map @Nothing@ to 'empty' and @Just t@ to @t@.
fromMaybe :: Maybe (MultiTrie v d) -> MultiTrie v d
fromMaybe = maybe empty id

-- | Map 'empty' to @Nothing@ and a non-empty @t@ to @Just t@.
toMaybe ::
    MultiTrie v d ->
    Maybe (MultiTrie v d)
toMaybe t = if null t then Nothing else Just t

-- | Convert a 'MultiTrie' into an ASCII-drawn tree.
draw :: (Show v, Show [d]) =>
    MultiTrie v d ->
    String
draw = T.drawTree . toTree show show

-- | Decide if maps are equivalent up to a custom value equivalence predicate.
-- True if and only if the maps have exactly the same names and, for each name,
-- its values in the two maps are equivalent. `Data.Map` is missing this.
areMapsEquivalentUpTo :: Ord k =>
    (a -> b -> Bool) ->
    M.Map k a ->
    M.Map k b ->
    Bool
areMapsEquivalentUpTo p m1 m2 = mapEquivalenceHelper
    (M.minViewWithKey m1)
    (M.minViewWithKey m2)
  where
    mapEquivalenceHelper Nothing Nothing = True
    mapEquivalenceHelper _ Nothing = False
    mapEquivalenceHelper Nothing _ = False
    mapEquivalenceHelper (Just ((k1, v1), m1')) (Just ((k2, v2), m2')) =
        k1 == k2 &&
        p v1 v2 &&
        areMapsEquivalentUpTo p m1' m2'

--
-- Internal helper functions
--

nullToEmpty ::
    MultiTrie v d ->
    MultiTrie v d
nullToEmpty t = if null t then empty else t

zipContentsAndChildren :: Ord v =>
    ([d] -> [d] -> [d]) ->
    (MultiTrieMap v d -> MultiTrieMap v d -> MultiTrieMap v d) ->
    MultiTrie v d ->
    MultiTrie v d ->
    MultiTrie v d
zipContentsAndChildren f g (MultiTrie ds1 m1) (MultiTrie ds2 m2) =
    MultiTrie (f ds1 ds2) (g m1 m2) 

toTree ::
    (v -> t) ->
    ([d] -> t) ->
    MultiTrie v d ->
    T.Tree t
toTree f g (MultiTrie ds m) =
    T.Node (g ds) $ M.elems $ M.mapWithKey namedChildToTree m
    where
        namedChildToTree k t = T.Node (f k) [toTree f g t]

listAsMultiSetIntersection :: Eq a =>
    [a] ->
    [a] ->
    [a]
listAsMultiSetIntersection [] _ = []
listAsMultiSetIntersection _ [] = []
listAsMultiSetIntersection (x:xs) ys = if x `L.elem` ys
    then x : listAsMultiSetIntersection xs (L.delete x ys)
    else listAsMultiSetIntersection xs ys

listAsMultiSetEquals :: Eq a =>
    [a] ->
    [a] ->
    Bool
listAsMultiSetEquals [] [] = True
listAsMultiSetEquals [] _ = False
listAsMultiSetEquals _ [] = False
listAsMultiSetEquals (x:xs) ys = if x `L.elem` ys
    then listAsMultiSetEquals xs (L.delete x ys)
    else False
