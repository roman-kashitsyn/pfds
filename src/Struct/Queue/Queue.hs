module Struct.Queue.Queue (PersistentQueue, fromList) where

import Prelude hiding (head, tail, foldr, foldl, concat)
import Data.Monoid
import Control.Applicative
import Data.Foldable
import Data.Traversable
import Struct.Interfaces.Queue

data PersistentQueue a = PQ [a] [a] Int Int

fromList :: [a] -> PersistentQueue a
fromList xs = PQ xs [] (length xs) 0

queue :: PersistentQueue a -> PersistentQueue a
queue q@(PQ f r fc rc) = if fc <= rc
                           then q
                           else PQ (f ++ reverse r) [] (fc + rc) 0

instance Queue PersistentQueue where
    head (PQ [] _ _ _) = error "Can not take head of an empty queue"
    head (PQ (x:_) _ _ _) = x

    tail (PQ [] _ _ _) = error "Can not take tail of an empty queue"
    tail (PQ (_:xs) r fc rc) = queue $ PQ xs r (pred fc) rc

    snoc e (PQ front rest fc rc) = queue $ PQ front (e:rest) fc (succ rc)

    isEmpty (PQ _ _ fc _) = fc == 0

instance Show a => Show (PersistentQueue a) where
    show = ("PersistentQueue " ++) . show . toList

instance Functor PersistentQueue where
    f `fmap` (PQ front rest fc rc) = PQ (f `fmap` front) (f `fmap` rest) fc rc

instance Foldable PersistentQueue where
    foldMap _ (PQ [] _ _ _) = mempty
    foldMap f (PQ front rest _ _) = (foldMap f front) `mappend` (foldMap f $ reverse rest)

instance Traversable PersistentQueue where
    traverse f (PQ front rest fc rc) = (\ f' r' -> PQ f' r' fc rc) <$> traverse f front <*> traverse f (reverse rest)
