-- | An implementation of list based on skew number model.
--   Declares skew list as instances of classes: Show, Functor, Foldable, Traversable.
--   Module is desingned for quilified import.
module Struct.List.SkewList (
                        SkewList
                        , empty   -- ^ Constructs empty skew list.
                        , isEmpty -- ^ Checks if list is empty.     Complexity: O(1).
                        , add     -- ^ Adds an element to a list.   Complexity: O(1).
                        , head    -- ^ Returns head of a list.      Complexity: O(1).
                        , tail    -- ^ Returns tail of a list.      Complexity: O(1).
                        , lookup  -- ^ Lookup an element by index.  Complexity: O(log n).
                        , update  -- ^ Updates an element at index. Complexity: O(log n).
                        , fromList -- ^ Constructs skew list from a regular list
                        , length   -- ^ Calculates length of a list. Complexity: O(log n).
                        , Data.Foldable.toList
                        ) where

import Prelude as P hiding (length, head, tail, concat, lookup)
import Control.Applicative hiding (empty)
import Data.Monoid()
import Data.Foldable
import Data.Traversable
import qualified Struct.Interfaces.IndexedList as I
import Struct.Internal.CompleteBinaryTree

newtype SkewList a = SL { contents ::[(Int, Tree a)] }

empty :: SkewList a
empty = SL []

isEmpty :: SkewList a -> Bool
isEmpty = P.null . contents

fromList :: [a] -> SkewList a
fromList = P.foldr add empty

add :: a -> SkewList a -> SkewList a
add x = SL . add' . contents
    where add' ts@((w1, t1):(w2, t2):ts') = if w1 == w2
                                              then (1 + w1 + w2, Node x t1 t2):ts'
                                              else (1, Leaf x):ts
          add' ts = (1, Leaf x):ts

head :: SkewList a -> a
head = head' . contents
    where head' [] = P.error "Can't take head of empty skew list"
          head' ((1, (Leaf x)):_) = x
          head' ((_, (Node x _ _)):_) = x

tail :: SkewList a -> SkewList a
tail = SL . tail' . contents
    where tail' [] = P.error "Can't take tail of empty skew list"
          tail' ((1, (Leaf _)):ts) = ts
          tail' ((w, (Node _ l r)):ts) = let w' = w `div` 2 in (w', l):(w', r):ts

indexError :: String -> Int -> t
indexError prefix idx = error $ prefix ++ ": index " ++ show idx ++ " out of list bounds"

lookup :: Int -> SkewList a -> a
lookup idx = lookup' idx . contents
    where lookup' _ [] = indexError "lookup" idx
          lookup' i ((w, t):ts) = if i < w then lookupTree w t i
                                  else lookup' (i - w) ts
          lookupTree 1 (Leaf x) 0 = x
          lookupTree 1 (Leaf _) i = indexError ("Leaf " ++ show i) idx
          lookupTree _ (Node x _ _) 0 = x
          lookupTree w (Node _ l r) i = let w' = w `div` 2
                                        in if i <= w' then lookupTree w' l (i - 1)
                                           else lookupTree w' r (i - 1 - w')

length :: SkewList a -> Int
length = foldl' (+) 0 . map (count . snd) . contents

update :: Int -> a -> SkewList a -> SkewList a
update idx e = SL . update' idx . contents
    where update' _ [] = indexError "update" idx
          update' i ((w, t):ts) = if i < w then (w, (updateTree w t i)):ts
                                  else (w, t):(update' (i - w) ts)
          updateTree 1 (Leaf _) 0 = Leaf e
          updateTree 1 (Leaf _) _ = indexError "updateTree" idx
          updateTree _ (Node _ l r) 0 = Node e l r
          updateTree w (Node x l r) i = let w' = w `div` 2
                                        in if i <= w' then Node x (updateTree w' l (pred i)) r
                                           else Node x l (updateTree w' r (i - 1 - w'))

instance I.IndexedList SkewList where
    isEmpty = isEmpty
    head = head
    tail = tail
    add = add
    lookup = lookup
    update = update

instance Show a => P.Show (SkewList a) where
    show = ("SkewList " ++) . show . toList

instance P.Functor SkewList where
    fmap f = SL . fmap' . contents
        where fmap' = map mapTree
              mapTree (w, t) = (w, f `fmap` t)

instance Foldable SkewList where
    foldMap f = foldMap id . map (foldMap f . snd) . contents

instance Traversable SkewList where
   traverse f sl = SL <$> traverse cpy lst
       where cpy (w, t) = (,) w <$> traverse f t
             lst = contents sl
