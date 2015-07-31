module Data.Allable (
    allValues
) where

allValues :: (Bounded a, Enum a) => [a]
allValues = [minBound..]

