module SkewHeapProps where

import Data.List (sort)
import Test.QuickCheck
import qualified Struct.Heap.SkewHeap as H

prop_findMin :: [Integer] -> Gen Prop
prop_findMin xs = (not $ null xs) ==> (H.findMin $ H.fromList xs) == minimum xs

prop_sorted :: [Integer] -> Bool
prop_sorted xs = (H.toList $ H.fromList xs) == sort xs

prop_merged :: ([Integer], [Integer]) -> Bool
prop_merged (xs, ys) = let h1 = H.fromList xs
                           h2 = H.fromList ys
                           merged = h1 `H.merge` h2
                           sorted = sort $ xs ++ ys
                       in H.toList merged == sorted
                           