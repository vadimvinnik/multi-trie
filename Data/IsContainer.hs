module Data.IsContainer where

class Unitable s where
    null :: s a -> Bool
    empty :: s a
    singleton :: a -> s a
    insert :: a -> s a -> s a
    union :: s a -> s a -> s a

class Intersectible s where
    repeat :: a -> s a
    intersection :: s a -> s a -> s a

class Topable s where
    top :: s a
    
