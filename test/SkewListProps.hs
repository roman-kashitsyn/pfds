module SkewListProps where

import Test.QuickCheck
import qualified Struct.List.SkewList as L

prop_listConversion :: [Integer] -> Bool
prop_listConversion l = l == (L.toList . L.fromList $ l)

prop_randomAccessListModel :: [Integer] -> Bool
prop_randomAccessListModel l =
    let indexedList     = L.fromList l
        range           = length l - 1
        accessIdx i     = l !! i == L.lookup i indexedList
    in all (== True) $ map accessIdx [0..range]

prop_lookupUpdated :: [Integer] -> Bool
prop_lookupUpdated l =
    let il               = L.fromList l
        range            = length l - 1
        lookupUpdated i n lst = n == (L.lookup i $ L.update i n lst)
    in all (== True) $ map (\ i -> lookupUpdated i (L.lookup i il + 1) il) [0..range]