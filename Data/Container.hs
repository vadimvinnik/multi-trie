module Data.Container where

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
    top :: (Bounded a, Enum a) => s a

