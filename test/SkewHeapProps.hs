module SkewHeapProps where

import Data.List (sort)
import Control.Applicative
import Test.QuickCheck
import qualified Struct.Heap.SkewHeap as H

instance (Arbitrary a, Ord a) => Arbitrary (H.SkewHeap a) where
    arbitrary = H.fromList <$> arbitrary

prop_findMin :: (Arbitrary a, Ord a, H.Heap h) => ([a] -> h a) -> [a] -> Gen Prop
prop_findMin fromList xs = (not $ null xs) ==> (H.findMin $ fromList xs) == minimum xs

prop_deleteMin :: (Arbitrary a, Ord a, H.Heap h) => ([a] -> h a) -> [a] -> Gen Prop
prop_deleteMin fromList xs = (not $ null xs) ==> let h = H.deleteMin $ fromList xs
                                                 in H.toList h == (tail $ sort xs)

prop_sorted :: (Arbitrary a, Ord a, H.Heap h) => ([a] -> h a) -> [a] -> Bool
prop_sorted fromList xs = (H.toList $ fromList xs) == sort xs

prop_merged :: (Arbitrary a, Ord a, H.Heap h) => ([a] -> h a) -> ([a], [a]) -> Bool
prop_merged fromList (xs, ys) = let h1 = fromList xs
                                    h2 = fromList ys
                                    merged = h1 `H.merge` h2
                                    sorted = sort $ xs ++ ys
                                in H.toList merged == sorted
                           