module Struct.Queue.Deque (PersistentDeque, fromList) where

import Prelude hiding (head, tail, init, last, foldr)
import Control.Applicative
import Data.Monoid
import Data.Foldable
import Data.Traversable
import Struct.Interfaces.Queue

loadFactor :: Int
loadFactor = 2

data PersistentDeque a = PD [a] [a] Int Int

balance :: [a] -> [a] -> Int -> ([a], [a])
balance xs ys l = let (xs', t) = splitAt l xs
                      ys'      = ys ++ reverse t
                  in (xs', ys')

queue :: PersistentDeque a -> PersistentDeque a
queue q@(PD f r fc rc)
  | fc > loadFactor * rc + 1 = let (f', r') = balance f r fc' in PD f' r' fc' rc'
  | rc > loadFactor * fc + 1 = let (r', f') = balance r f rc' in PD f' r' fc' rc'
  | otherwise                = q
  where len = fc + rc
        fc' = len `div` 2
        rc' = len- fc'

fromList :: [a] -> PersistentDeque a
fromList l = queue $ PD l [] (length l) 0

instance Queue PersistentDeque where
    head (PD [] _ _ _) = error "Can not take head of an empty queue"
    head (PD (x:_) _ _ _) = x

    tail (PD [] _ _ _) = error "Can not take tail of an empty queue"
    tail (PD (_:xs) r fc rc) = queue $ PD xs r (pred fc) rc

    snoc e (PD f r fc rc) = queue $ PD f (e:r) fc (succ rc)

    isEmpty (PD _ _ fc rc) = fc + rc == 0

instance Deque PersistentDeque where
    last (PD _ [] _ _) = error "Can not take last element of an empty dequeue"
    last (PD _ (x:_) _ _) = x

    init (PD _ [] _ _) = error "Can not take init of an empty dequeue"
    init (PD f (_:xs) fc rc) = queue $ PD f xs fc (pred rc)

    cons e (PD f r fc rc) = queue $ PD (e:f) r (succ fc) rc

instance Show a => Show (PersistentDeque a) where
    show = ("PersistentDeque " ++) . show . toList

instance Functor PersistentDeque where
    f `fmap` (PD front rest fc rc) = PD (f `fmap` front) (f `fmap` rest) fc rc

instance Foldable PersistentDeque where
    foldMap _ (PD [] [] _ _) = mempty
    foldMap f (PD front rest _ _) = (foldMap f front) `mappend` (foldMap f $ reverse rest)

instance Traversable PersistentDeque where
    traverse f (PD front rest fc rc) = (\ f' r' -> PD f' r' fc rc) <$> traverse f front <*> traverse f (reverse rest)
