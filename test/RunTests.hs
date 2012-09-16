module Main where

import SkewListProps
import SkewHeapProps
import qualified Struct.Heap.SkewHeap as SH

import Test.HUnit
import Test.QuickCheck
import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

mkSkewHeap :: Ord a => [a] -> SH.SkewHeap a
mkSkewHeap = SH.fromList

main :: IO ()
main = defaultMain tests

tests :: [TF.Test]
tests = [ testGroup "QuickCheck Struct.List.SkewList"
          [ testProperty "listConversion" prop_listConversion
          , testProperty "randomAccessListModel" prop_randomAccessListModel
          , testProperty "lookupUpdated" prop_lookupUpdated
          ]
        , testGroup "QuickCheck Struct.Heap.SkewHeap"
          [ testProperty "findMin" (prop_findMin mkSkewHeap :: [Int] -> Gen Prop)
          , testProperty "deleteMin" (prop_deleteMin mkSkewHeap :: [Int] -> Gen Prop)
          , testProperty "sorted" (prop_sorted mkSkewHeap :: [Int] -> Bool)
          , testProperty "merged" (prop_merged mkSkewHeap :: ([Int], [Int]) -> Bool)
          ]
        ]