{-# LANGUAGE FlexibleContexts #-}

{- |
A 'MultiTrie' @v d@ is a trie (i.e. a tree whose child nodes have distinct
labels of type @v@) with each node containing a list of values of type @d@
considered as a multiset.  It represents a multivalued naming with compound
names: each compound name (a chain of labels) has a (possibly empty) list of
values.

The simplest possible 'MultiTrie' is 'empty' that has an empty list of values
and an no children.  Since the only essential feature of a 'MultiTrie' is
carrying values, the 'empty' 'MultiTrie' could be equated with an absense of a
'MultiTrie'.  In particular, instead of saying that there is no sub-trie under
some path in a 'MultiTrie', let us say that the path points to an 'empty' node.
Therefore, every 'MultiTrie' could be considered as infinite, where each node
has children under all possible names - and some of nodes are 'empty'.

Some operations could be defined for 'MultiTrie's in a natural way, including
'map', 'union', 'intersection', 'cartesian'.  Obviously, 'empty' is a
neutral element of 'union'.  Cartesian product is 'empty' if any of the two
operands is 'empty'.

A unary function can be applied to each value in each node of a 'MultiTrie'.
Moreover, a 'MultiTrie' can contain not only ordinary values but also functions
that makes it possible to apply a 'MultiTrie' of functions to a 'MultiTrie' of
argument values, combining results with 'cartesian'.  A 'MultiTrie'
whose values are, in their turn,  'MultiTrie's, can be 'flatten'ed.  This makes
'MultiTrie's an instance of 'Functor', Applicative' and 'Monad' classes.

For a detailed description of a mathematical notion of multivalued naming with
compound names, its operations and properties, see an article distributed with
this package as a LaTeX source.
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
    isEqualStrict,
    isEqualWeak,
    areEquivalentUpTo,
    -- * Subnode access
    subnode,
    subnodeUpdate,
    subnodeAddValue,
    subnodeReplace,
    subnodeDelete,
    subnodeUnite,
    subnodeIntersect,
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

import Prelude hiding (null, repeat, map)
import qualified Data.Foldable as F
import qualified Data.Map as M
import qualified Data.Tree as T
import qualified Data.List as L
import Control.Applicative hiding (empty)

-- | A map of atomic names onto child nodes.
type MultiTrieMap n v = M.Map n (MultiTrie n v) 

-- | A trie consists of a list of values and labelled child tries.
data MultiTrie n v = MultiTrie
    {
        values :: [v],
        children :: MultiTrieMap n v
    }
    deriving (Show)

instance Ord n => Functor (MultiTrie n) where
    fmap = map

instance Ord n => Applicative (MultiTrie n) where
    pure = singleton
    (<*>) = apply

instance Ord n => Monad (MultiTrie n) where
    return = singleton
    (>>=) = bind

instance (Ord n, Eq v) => Eq (MultiTrie n v) where
    (==) = isEqualStrict

-- | An empty 'MultiTrie' constant. A neutral element of 'union'.
empty :: MultiTrie n v
empty = MultiTrie [] M.empty

-- | A 'MultiTrie' containing just one value in its root and no child nodes.
singleton :: v -> MultiTrie n v
singleton x = leaf [x]

-- | A 'MultiTrie' containing the given list in its root and no child nodes.
leaf ::
    [v] ->
    MultiTrie n v
leaf vs = MultiTrie vs M.empty

{- |
An infinite 'MultiTrie' that has in each node the same list of values and,
under each name from the given set, a child identical to the root.
-}
repeat :: Ord n =>
    [n] ->
    [v] ->
    MultiTrie n v
repeat ns xs =
    if   L.null xs
    then empty
    else MultiTrie xs (M.fromList $ zip ns $ L.repeat $ repeat ns xs)

-- | Change a list in the root node with a function and leave children intact.
updateValues ::
    ([v] -> [v]) ->
    MultiTrie n v ->
    MultiTrie n v
updateValues f (MultiTrie vs m) = MultiTrie (f vs) m

-- | Add a new value to the root node's list of values.
addValue ::
    v ->
    MultiTrie n v ->
    MultiTrie n v
addValue v = updateValues (v:)

-- | Check whether a 'MultiTrie' is empty.
null ::
    MultiTrie n v ->
    Bool
null (MultiTrie vs m) = L.null vs && L.all null (M.elems m)

-- | A total number of values in all nodes.
size ::
    MultiTrie n v ->
    Int
size (MultiTrie vs m) = L.length vs + L.sum (L.map size (M.elems m))

-- | Check for equality counting the order of elements.
isEqualStrict :: (Ord n, Eq v) =>
    MultiTrie n v ->
    MultiTrie n v ->
    Bool
isEqualStrict = areEquivalentUpTo (==)

-- | Check for equality ignoring the order of elements.
isEqualWeak :: (Ord n, Eq v) =>
    MultiTrie n v ->
    MultiTrie n v ->
    Bool
isEqualWeak = areEquivalentUpTo listAsMultiSetEquals

{- |
Decide whether two 'MultiTrie's @u@ and @v@ are equivalent up to a custom list
equivalence predicate @p@.
True if and only if (1) the 'MultiTrie's have non-empty nodes at the same paths
and (2) for each such path @s@, value lists from @u@ and @v@ under @p@ are
equivalent, i.e. satisfy @p@.
-}
areEquivalentUpTo :: (Ord n, Eq v) =>
    ([v] -> [v] -> Bool) ->
    MultiTrie n v ->
    MultiTrie n v ->
    Bool
areEquivalentUpTo p (MultiTrie vs1 m1) (MultiTrie vs2 m2) =
    (p vs1 vs2) &&
    (areMapsEquivalentUpTo (areEquivalentUpTo p) m1 m2)

{-
Select a 'MultiTrie' subnode identified by the given path, or 'empty' if there
is no such path.
-}
subnode :: Ord n =>
    [n] ->
    MultiTrie n v ->
    MultiTrie n v
subnode [] mt = mt
subnode (n:ns) (MultiTrie _ m) = maybe empty (subnode ns) (M.lookup n m)

-- | Perform the given transformation on a subnode identified by the path.
subnodeUpdate :: Ord n =>
    [n] ->
    (MultiTrie n v -> MultiTrie n v) ->
    MultiTrie n v ->
    MultiTrie n v
subnodeUpdate [] f mt = f mt
subnodeUpdate (n:ns) f (MultiTrie vs m) =
    MultiTrie vs (M.alter (toMaybe . subnodeUpdate ns f . fromMaybe) n m)

-- | Add a value to a list of values in a subnode identified by the path.
subnodeAddValue :: Ord n =>
    [n] ->
    v ->
    MultiTrie n v ->
    MultiTrie n v
subnodeAddValue ns v = subnodeUpdate ns (addValue v)

-- | Replace a subnode identified by the path with a new 'MultiTrie'.
subnodeReplace :: Ord n =>
    [n] ->
    MultiTrie n v ->
    MultiTrie n v ->
    MultiTrie n v
subnodeReplace ns mt1 = subnodeUpdate ns (const mt1)

-- | Delete a subnode identified by the given path.
subnodeDelete :: Ord n =>
    [n] ->
    MultiTrie n v ->
    MultiTrie n v
subnodeDelete ns = subnodeReplace ns empty

-- | Unite a subnode identified by the path with another 'MultiTrie'.
subnodeUnite :: Ord n =>
    [n] ->
    MultiTrie n v ->
    MultiTrie n v ->
    MultiTrie n v
subnodeUnite ns mt1 = subnodeUpdate ns (union mt1)

-- | Intersect a subnode identified by the path with another 'MultiTrie'.
subnodeIntersect :: (Ord n, Eq v) =>
    [n] ->
    MultiTrie n v ->
    MultiTrie n v ->
    MultiTrie n v
subnodeIntersect ns mt1 = subnodeUpdate ns (intersection mt1)

-- | Map a function over all values in a 'MultiTrie'.
map :: Ord n =>
    (v -> w) ->
    MultiTrie n v ->
    MultiTrie n w
map f = mapOnLists (L.map f)

-- | Map a function over all values, passing node paths as well.
mapWithName :: Ord n =>
    ([n] -> v -> w) ->
    MultiTrie n v ->
    MultiTrie n w
mapWithName f = mapOnListsWithName (L.map . f) 

-- | Apply a list of functions to all values in a 'MultiTrie'.
mapMany :: Ord n =>
    [v -> w] ->
    MultiTrie n v ->
    MultiTrie n w
mapMany fs  = mapOnLists (fs <*>)

-- | Apply a list of functions to each value and its path.
mapManyWithName :: Ord n =>
    [[n] -> v -> w] ->
    MultiTrie n v ->
    MultiTrie n w
mapManyWithName fs = mapOnListsWithName (\ns -> (L.map ($ns) fs <*>))

-- | Map a function over entire lists contained in nodes.
mapOnLists :: Ord n =>
    ([v] -> [w]) ->
    MultiTrie n v ->
    MultiTrie n w
mapOnLists fl (MultiTrie vs vm) =
    MultiTrie (fl vs) (M.mapMaybe (toMaybe . mapOnLists fl) vm)

-- | Map a function over entire lists in nodes, passing node path as well.
mapOnListsWithName :: Ord n =>
    ([n] -> [v] -> [w]) ->
    MultiTrie n v ->
    MultiTrie n w
mapOnListsWithName fl (MultiTrie vs vm) =
    MultiTrie
        (fl [] vs)
        (M.mapMaybeWithKey transformChild vm) where
    transformChild n = toMaybe . (mapOnListsWithName $ fl . (n:))

{- |
Cartesian product of two 'MultiTrie's. The resulting 'MultiTrie' consists of
all possible pairs @(x,y)@ under a concatenated name @u ++ v@ where @x@ is a
value contained in the first operand under a name @u@, and @y@ is contained in
the second operand under a name @v@.
-}
cartesian :: Ord n =>
    MultiTrie n v ->
    MultiTrie n w ->
    MultiTrie n (v, w)
cartesian mtv = apply (map (,) mtv)

-- | Union of 'MultiTrie's.
union :: Ord n =>
    MultiTrie n v ->
    MultiTrie n v ->
    MultiTrie n v
union = zipContentsAndChildren (++) (M.unionWith union)

-- | Union of a list of 'MultiTrie's.
unions :: Ord n =>
    [MultiTrie n v] ->
    MultiTrie n v
unions = L.foldl union empty

-- | Intersection of 'MultiTrie's.
intersection :: (Ord n, Eq v) =>
    MultiTrie n v ->
    MultiTrie n v ->
    MultiTrie n v
intersection mt = nullToEmpty .
    zipContentsAndChildren
        listAsMultiSetIntersection
        (\x y -> M.filter (not . null) (M.intersectionWith intersection x y))
        mt 

-- | Intersection of a non-empty list of 'MultiTrie's.
intersections1 :: (Ord n, Eq v) =>
    [MultiTrie n v] ->
    MultiTrie n v
intersections1 = L.foldl1 intersection

-- | Flatten a 'MultiTrie' whose values are, in their turn, 'MultiTrie's
flatten :: Ord n =>
    MultiTrie n (MultiTrie n v) ->
    MultiTrie n v
flatten (MultiTrie mts mtm) =
    F.foldr union empty mts `union` MultiTrie [] (M.map flatten mtm)

{- |
Given a 'MultiTrie' of functions and a 'MultiTrie' of values, apply each
function from the first operand to each value from the second operand and
combining results as in 'cartesian': under a name concatenated from the
function's and value's names.
-}
apply :: Ord n =>
    MultiTrie n (v -> w) ->
    MultiTrie n v ->
    MultiTrie n w
apply mtf mtx = flatten $ map (`map` mtx) mtf

{-
Given a multitree @P@ of values and a function @f@ that maps an arbitrary value
to a multitree, apply the function @f@ to each value from @P@ and combine the
results as described in 'flatten'.
-}
bind :: Ord n =>
    MultiTrie n v ->
    (v -> MultiTrie n w) ->
    MultiTrie n w
bind mt fmt = flatten $ map fmt mt

{-
Convert a 'MultiTrie' @P@ to a map @M@ whose keys are paths from @P@, and values
in @M@ are respective lists representing lists of values in @P@.
-}
toMap :: Ord n =>
    MultiTrie n v ->
    M.Map [n] [v]
toMap (MultiTrie vs m) = if L.null vs
        then childrenMap
        else M.insert [] vs childrenMap
    where
        childrenMap =
            M.unions $
            M.elems $
            M.mapWithKey (\n -> M.mapKeys (n:)) $
            M.map toMap m

-- | Convert a 'MultiTrie' to a list of path-value pairs.
toList :: Ord n =>
    MultiTrie n v ->
    [([n], v)]
toList (MultiTrie vs m) = (L.map ((,) []) vs) ++
    (
        L.concat $
        L.map (\(n, ps) -> L.map (\(ns, ws) -> (n:ns, ws)) ps) $
        M.toList $
        M.map toList m
    )

-- | Convert a list of path-value pairs to a 'MultiTrie'.
fromList :: Ord n =>
    [([n], v)] ->
    MultiTrie n v
fromList = L.foldr (uncurry subnodeAddValue) empty

-- | Map 'Nothing' to 'empty' and @Just mt@ to @mt@.
fromMaybe :: Maybe (MultiTrie n v) -> MultiTrie n v
fromMaybe = maybe empty id

-- | Map 'empty' to 'Nothing' and a non-empty @mt@ to @Just mt@.
toMaybe ::
    MultiTrie n v ->
    Maybe (MultiTrie n v)
toMaybe mt = if null mt then Nothing else Just mt

-- | Convert a 'MultiTrie' into an ASCII-drawn tree.
draw :: (Show n, Show [v]) =>
    MultiTrie n v ->
    String
draw = T.drawTree . toTree show show

{- |
Decide whether maps are equivalent up to a custom value equivalence predicate.
True if and only if the maps have exactly the same names and, for each name,
its values in the two maps are equivalent. `Data.Map` is missing this.
-}
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
    MultiTrie n v ->
    MultiTrie n v
nullToEmpty mt = if null mt then empty else mt

zipContentsAndChildren :: Ord n =>
    ([v] -> [v] -> [v]) ->
    (MultiTrieMap n v -> MultiTrieMap n v -> MultiTrieMap n v) ->
    MultiTrie n v ->
    MultiTrie n v ->
    MultiTrie n v
zipContentsAndChildren f g (MultiTrie vs1 m1) (MultiTrie vs2 m2) =
    MultiTrie (f vs1 vs2) (g m1 m2) 

toTree ::
    (n -> t) ->
    ([v] -> t) ->
    MultiTrie n v ->
    T.Tree t
toTree f g (MultiTrie vs m) =
    T.Node (g vs) $ M.elems $ M.mapWithKey namedChildToTree m
    where
        namedChildToTree k mt = T.Node (f k) [toTree f g mt]

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

