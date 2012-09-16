module Struct.Heap.SkewHeap ( SkewHeap
                            , H.Heap(..)
                            , fromList
                            , F.toList
                            ) where

import Prelude as P
import Data.Monoid
import Data.Foldable as F
import qualified Struct.Interfaces.Heap as H
import Struct.Internal.SkewBinomialTree

newtype SkewHeap a = SH { contents :: [Tree a] }

empty :: SkewHeap a
empty = SH []

isEmpty :: SkewHeap a -> Bool
isEmpty = null . contents

insertTree :: Ord a => Tree a -> [Tree a] -> [Tree a]
insertTree t [] = [t]
insertTree t (t':ts) = if rank t < rank t'
                         then t:t':ts
                         else insertTree (link t t') ts

mergeTrees :: Ord a => [Tree a] -> [Tree a] -> [Tree a]
mergeTrees ts [] = ts
mergeTrees [] ts = ts
mergeTrees l@(t:ts) l'@(t':ts') = case rank t `compare` rank t' of
                                    LT -> t  : (mergeTrees ts l')
                                    GT -> t' : (mergeTrees l ts')
                                    EQ -> insertTree (link t t') (mergeTrees ts ts')

normalize :: Ord a => [Tree a] -> [Tree a]
normalize [] = []
normalize (t:ts) = insertTree t ts

insert' :: Ord a => a -> [Tree a] -> [Tree a]
insert' x ts@(t1:t2:rest) = if rank t1 == rank t2
                              then (skewLink x t1 t2):rest
                              else (Node 0 x [] []):ts
insert' x ts = (Node 0 x [] []):ts

insert :: Ord a => a -> SkewHeap a -> SkewHeap a
insert x = SH . insert' x . contents

merge :: Ord a => SkewHeap a -> SkewHeap a -> SkewHeap a
merge h h' = SH $ mergeTrees (normalizeH h) (normalizeH h')
    where normalizeH = normalize . contents

findMin :: Ord a => SkewHeap a -> a
findMin = min' . contents
    where min' [] = error "Can not take minimum of empty heap"
          min' [t] = root t
          min' (t:ts) = let x = root t
                            y = findMin (SH ts)
                        in min x y

deleteMin :: Ord a => SkewHeap a -> SkewHeap a
deleteMin = SH . del' . contents
    where del' [] = error "Can not delete minimum of empty heap"
          del' ts = let getMin [t] = (t, [])
                        getMin (t:ts) = let (t', ts') = getMin ts
                                        in if root t < root t' then (t, ts) else (t', t:ts')
                        (Node _ _ xs c, ts') = getMin ts
                        insertAll [] ts = ts
                        insertAll (t:ts) ts' = insertAll ts (insert' t ts')
                    in insertAll xs $ mergeTrees (reverse c) (normalize ts')

fromList :: Ord a => [a] -> SkewHeap a
fromList = P.foldr insert empty

instance Show a => Show (SkewHeap a) where
    show h = "SkewHeap " ++ (show . P.concat . map toList . contents $ h)

instance Ord a => Monoid (SkewHeap a) where
    mempty = empty
    mappend = merge

instance H.Heap SkewHeap where
    empty = empty
    isEmpty = isEmpty
    insert = insert
    merge = merge
    findMin = findMin
    deleteMin = deleteMin

instance Foldable SkewHeap where
    foldMap f h
        | isEmpty h = mempty
        | otherwise = foldMap id . map (foldMap f) . contents $ h
