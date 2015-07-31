module Data.Container where

import qualified Data.Set as S
import qualified Data.MultiSet as M
import qualified Data.List as L

class Elementary s where
    null :: s a -> Bool
    elem :: Eq a => a -> s a -> Bool
    empty :: s a
    singleton :: a -> s a

class Elementary s => Insertable s where
    insert :: a -> s a -> s a

class Elementary s => Deletable s where
    delete :: Eq a => a -> s a -> s a

class Elementary s => Unitable s where
    union :: s a -> s a -> s a

class Elementary s => Intersectible s where
    intersection :: Eq a => s a -> s a -> s a

class Elementary s => Subtractible s where
    difference :: Eq a => s a -> s a -> s a

class Intersectible s => Fullable s where
    full :: a -> s a

class Intersectible s => Topable s where
    top :: s a

instance Elementary [] where
    null = L.null
    elem = L.elem
    empty = []
    singleton x = [x]

instance Insertable [] where
    insert = (:)

instance Deletable [] where
    delete = L.delete

    union = (++)

instance Fullable [] where
    full = L.repeat
