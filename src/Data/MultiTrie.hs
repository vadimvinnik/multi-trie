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

The opposite to the 'empty' is a 'top' multi-trie whose each node contains
the greatest possible multi-set of the element type's values (i.e. an infinite
list that has infinite instances of every of the element type's values) under
every possible path.

Some operations could be defined for multi-tries in a rather natural way,
including 'map', 'union', 'intersection', 'cartesianProduct'.

Obviously, 'empty' is a neutral element of union, and 'top' is that for
intersection. Cartesian product is 'empty' if any of the two operands is
'empty'. Dually, from the pure mathematical perspective, cartesian square of
'empty' is 'empty'; although this cannot be implemented in this library within
finite time and memory.

Moreover, a multi-trie can contain not only ordinary values but also functions
that makes it possible to apply a multi-trie of functions to a multi-trie of
argument values, combining results with 'cartesianProduct', 'union' or
'intersection'. It makes multi-tries an instance of 'Applicative' and 'Monad'.

Implementaion notes. There are @Set@ and @Multiset@ and @Set@ types already
implemented. However, this library only uses lists. The reason is that
(Multi)Set requires its element type to be order-comparable, i.e. an instance
of @Ord@ that is obviously not the case for functions. In this library's
design, flexibility was the main priority, and a decision was made to sacrifice
some efficiency for the sake of genericity.
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
    updateValues,
    add,
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
    updateSubnode,
    addToSubnode,
    replaceSubnode,
    deleteSubnode,
    uniteSubnode,
    intersectSubnode,
    -- * Mappings
    mtmap,
    mtmapWithPath,
    mtmapAll,
    mtmapAllWithPath,
    mtmapContainers,
    mtmapContainersWithPath,
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
    draw,
    -- * Other
    listAsMultiSetEquals,
    areMapsEquivalentUpTo 
) where

import Prelude hiding (null, repeat)
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

instance (Ord n, Eq v) => Eq (MultiTrie n v) where
    (==) = isEqualStrict

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

Use with causion! Consumes lots of memory even despite laziness. Consider using
it only if both @n@ and @v@ types have less than dozen of values like Bool.
-}
top :: (Ord n, Bounded n, Enum n, Bounded v, Enum v) => MultiTrie n v
top = repeat allValues $ L.cycle allValues

-- | Change a multiset in the root node
updateValues :: ([v] -> [v]) -> MultiTrie n v -> MultiTrie n v
updateValues f (MultiTrie vs m) = MultiTrie (f vs) m

-- | Add a new value to the root node's multiset of values.
add :: v -> MultiTrie n v -> MultiTrie n v
add v = updateValues (v:)

-- | Check whether a multi-trie is empty.
null :: MultiTrie n v -> Bool
null (MultiTrie vs m) = L.null vs && L.all null (M.elems m)

-- | A total number of values in all nodes.
size :: MultiTrie n v -> Int
size (MultiTrie vs m) = L.length vs + L.sum (L.map size (M.elems m))

-- | Check for equality, order of elements is important
isEqualStrict :: (Ord n, Eq v) => MultiTrie n v -> MultiTrie n v -> Bool
isEqualStrict = areEquivalentUpTo (==)

-- | Check for equality, ignore order of elements
isEqualWeak :: (Ord n, Eq v) => MultiTrie n v -> MultiTrie n v -> Bool
isEqualWeak = areEquivalentUpTo listAsMultiSetEquals

{- |
Decide whether two multi-tries @u@ and @v@ are equivalent up to a custom list
equivalence predicate @p@.
True if and only if (1) the multi-tries have non-empty nodes at the same paths
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
Select a multi-trie subnode identified by the given path, or 'empty' if there
is no such path.
-}
subnode :: Ord n => [n] -> MultiTrie n v -> MultiTrie n v
subnode [] mt = mt
subnode (n:ns) (MultiTrie _ m) = maybe empty (subnode ns) (M.lookup n m)

-- | Perform the given transformation on a subnode identified by the path.
updateSubnode :: Ord n =>
    [n] ->
    (MultiTrie n v -> MultiTrie n v) ->
    MultiTrie n v ->
    MultiTrie n v
updateSubnode [] f mt = f mt
updateSubnode (n:ns) f (MultiTrie vs m) =
    MultiTrie vs (M.alter (toMaybe . updateSubnode ns f . fromMaybe) n m)

-- | Add a value to a multiset of values in a subnode identified by the path.
addToSubnode :: Ord n => [n] -> v -> MultiTrie n v -> MultiTrie n v
addToSubnode ns v = updateSubnode ns (add v)

-- | Replace a subnode identified by the path with a new multi-trie.
replaceSubnode :: Ord n => [n] -> MultiTrie n v -> MultiTrie n v -> MultiTrie n v
replaceSubnode ns mt1 = updateSubnode ns (const mt1)

-- | Delete a subnode identified by the given path.
deleteSubnode :: Ord n => [n] -> MultiTrie n v -> MultiTrie n v
deleteSubnode ns = replaceSubnode ns empty

-- | Unite a subnode identified by the path with another multi-trie.
uniteSubnode :: Ord n =>
    [n] ->
    MultiTrie n v ->
    MultiTrie n v ->
    MultiTrie n v
uniteSubnode ns mt1 = updateSubnode ns (union mt1)

-- | Intersect a subnode identified by the path with another multi-trie.
intersectSubnode :: (Ord n, Eq v) =>
    [n] ->
    MultiTrie n v ->
    MultiTrie n v ->
    MultiTrie n v
intersectSubnode ns mt1 = updateSubnode ns (intersection mt1)

-- | Map a function over all values in a multi-trie.
mtmap :: Ord n => (v -> w) -> MultiTrie n v -> MultiTrie n w
mtmap f = mtmapContainers (L.map f)

-- | Map a function over all values, passing node paths as well.
mtmapWithPath :: Ord n => ([n] -> v -> w) -> MultiTrie n v -> MultiTrie n w
mtmapWithPath f = mtmapContainersWithPath (L.map . f) 

{-
Apply a multiset @F@ of functions to all values in a multi-trie. If @V@ is a
multi-set of values under a certain path @s@ in a multi-trie @P@, the result
@Q@ will contain under @s@ a multi-set of all @(f v)@ values, for all @v@ from
@V@ and all @f@ from F.
-}
mtmapAll :: Ord n => [v -> w] -> MultiTrie n v -> MultiTrie n w
mtmapAll fs  = mtmapContainers (fs <*>)

-- | Apply a multiset of functions to each value and its path.
mtmapAllWithPath :: Ord n => [[n] -> v -> w] -> MultiTrie n v -> MultiTrie n w
mtmapAllWithPath fs = mtmapContainersWithPath (\ns -> (L.map ($ns) fs <*>))

-- | Map a function over entire multisets contained in nodes.
mtmapContainers :: Ord n => ([v] -> [w]) -> MultiTrie n v -> MultiTrie n w
mtmapContainers fl (MultiTrie vs vm) =
    MultiTrie (fl vs) (M.mapMaybe (toMaybe . mtmapContainers fl) vm)

-- | Map a function over entire multisets, passing node path as well.
mtmapContainersWithPath :: Ord n =>
    ([n] -> [v] -> [w]) ->
    MultiTrie n v ->
    MultiTrie n w
mtmapContainersWithPath fl (MultiTrie vs vm) =
    MultiTrie
        (fl [] vs)
        (M.mapMaybeWithKey transformChild vm) where
    transformChild n = toMaybe . (mtmapContainersWithPath $ fl . (n:))

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

-- | Intersection of an empty list of multi-tries.
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
applyIntersecting =
    applyZippingChildren (M.intersectionWith intersection)

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
fromList = L.foldr (uncurry addToSubnode) empty

-- | Map 'Nothing' to 'empty' and @Just mt@ to @mt@.
fromMaybe :: Maybe (MultiTrie n v) -> MultiTrie n v
fromMaybe = maybe empty id

-- | Map 'empty' to 'Nothing' and a non-empty @mt@ to @Just mt@.
toMaybe :: MultiTrie n v -> Maybe (MultiTrie n v)
toMaybe mt = if null mt then Nothing else Just mt

-- | Convert a multi-trie into an ASCII-drawn tree.
draw :: (Show n, Show [v]) => MultiTrie n v -> String
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
listAsMultiSetIntersection _ [] = []
listAsMultiSetIntersection (x:xs) ys = if x `L.elem` ys
    then x : listAsMultiSetIntersection xs (L.delete x ys)
    else listAsMultiSetIntersection xs ys

listAsMultiSetEquals :: Eq a => [a] -> [a] -> Bool
listAsMultiSetEquals [] [] = True
listAsMultiSetEquals [] _ = False
listAsMultiSetEquals _ [] = False
listAsMultiSetEquals (x:xs) ys = if x `L.elem` ys
    then listAsMultiSetEquals xs (L.delete x ys)
    else False

allValues :: (Bounded a, Enum a) => [a]
allValues = [minBound..]

