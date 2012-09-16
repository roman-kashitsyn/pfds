module Struct.Interfaces.IndexedList where

class IndexedList l where
    isEmpty :: l a -> Bool
    head :: l a -> a
    tail :: l a -> l a
    add :: a -> l a -> l a
    lookup :: Int -> l a -> a
    update :: Int -> a -> l a -> l a
