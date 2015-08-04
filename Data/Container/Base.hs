{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Container.Base where

class Elementary s where
    null :: s a -> Bool
    count :: s a -> Int
    empty :: s a
    singleton :: a -> s a

class Elementary s => Decidable a s where
    elem :: a -> s a -> Bool

class Elementary s => Mapable a b s where
    map :: (a -> b) -> s a -> s b

class Elementary s => Insertable a s where
    insert :: a -> s a -> s a

class (Elementary s, Eq a) => Deletable a s where
    delete :: a -> s a -> s a

class Elementary s => Unitable a s where
    union :: s a -> s a -> s a

class Elementary s => Intersectible a s where
    intersection :: s a -> s a -> s a

class Elementary s => Subtractible a s where
    difference :: s a -> s a -> s a

class Elementary s => Repeatable a s where
    repeat :: a -> s a

class Elementary s => Topable a s where
    top :: s a

