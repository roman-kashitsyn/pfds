module Main where

import SkewListProps
import SkewHeapProps

import Test.HUnit
import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)



main :: IO ()
main = defaultMain tests

tests :: [TF.Test]
tests = [ testGroup "QuickCheck Struct.List.SkewList"
          [ testProperty "listConversion" prop_listConversion
          , testProperty "randomAccessListModel" prop_randomAccessListModel
          , testProperty "lookupUpdated" prop_lookupUpdated
          ]
        , testGroup "QuickCheck Struct.Heap.SkewHeap"
          [ testProperty "findMin" prop_findMin
          , testProperty "sorted" prop_sorted
          , testProperty "merged" prop_merged
          ]
        ]