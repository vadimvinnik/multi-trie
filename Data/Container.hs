{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Container where

class Elementary a s where
    null :: s a -> Bool
    elem :: Eq a => a -> s a -> Bool
    empty :: s a
    singleton :: a -> s a

class Elementary a s => Insertable a s where
    insert :: a -> s a -> s a

class (Elementary a s, Eq a) => Deletable a s where
    delete :: a -> s a -> s a

class Elementary a s => Unitable a s where
    union :: s a -> s a -> s a

class (Elementary a s, Eq a) => Intersectible a s where
    intersection :: s a -> s a -> s a

class (Elementary a s, Eq a) => Subtractible a s where
    difference :: s a -> s a -> s a

class Elementary a s => Fullable a s where
    full :: a -> s a

class (Elementary a s, Bounded a, Enum a) => Topable a s where
    top :: s a

