module Struct.Internal.SkewBinomialTree where

import Data.Monoid
import Data.Foldable

data Tree a = Node Int a [a] [Tree a]

rank (Node r _ _ _) = r

root (Node _ x _ _) = x

link :: Ord a => Tree a -> Tree a -> Tree a
link t@(Node r x xs c) t'@(Node _ x' xs' c') = if x <= x'
                                                 then Node (succ r) x xs (t':c)
                                                 else Node (succ r) x' xs' (t:c')

skewLink :: Ord a => a -> Tree a -> Tree a -> Tree a
skewLink x t t' = let (Node r y ys c) = link t t'
                      minXY = min x y
                      maxXY = max x y
                  in Node r minXY (maxXY:ys) c

instance Foldable Tree where
    foldMap f (Node _ x xs ts) = f x `mappend` foldMap f xs
                                     `mappend` (foldMap id . map (foldMap f) $ ts)