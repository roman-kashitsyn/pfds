module Struct.Interfaces.Heap where

class Heap h where
    empty :: h a
    isEmpty :: h a -> Bool
    insert :: Ord a => a -> h a -> h a
    merge :: Ord a => h a -> h a -> h a
    findMin :: Ord a => h a -> a
    deleteMin :: Ord a => h a -> h a

foldrHeap :: (Ord a, Heap h) => (a -> b -> b) -> b -> h a -> b
foldrHeap f z h
        | isEmpty h = z
        | otherwise = foldrHeap f (f (findMin h) z) $ deleteMin h

toList :: (Ord a, Heap h) => h a -> [a]
toList h | isEmpty h = []
         | otherwise = (findMin h) : (toList $ deleteMin h)

fromList :: (Ord a, Heap h) => [a] -> h a
fromList = foldr insert empty
