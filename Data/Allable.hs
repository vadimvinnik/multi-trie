module Data.Allable (
    Allable,
    allValues
) where

class (Bounded a, Enum a) => Allable a where
    allValues :: [a]
    allValues = [minBound..]

