-- | Contains implementation of complete binary tree.
--   Declares the tree type as instance of type classes
--   Functor, Foldable, Traversable.
module Struct.Internal.CompleteBinaryTree where

import Prelude hiding (foldr)
import Control.Applicative
import Data.Monoid
import Data.Foldable
import Data.Traversable

data Tree a = Leaf a | Node a (Tree a) (Tree a)
    deriving (Show)

count :: Tree a -> Int
count = foldr (+) 0 . fmap (const 1)

instance Functor Tree where
    fmap f (Leaf x) = Leaf $ f x
    fmap f (Node x l r) = Node (f x) (f `fmap` l) (f `fmap` r) 

instance Foldable Tree where
    foldMap f (Leaf x) = f x
    foldMap f (Node x l r) = f x `mappend` (foldMap f l) `mappend` (foldMap f r)

instance Traversable Tree where
    traverse f (Leaf x) = Leaf <$> f x
    traverse f (Node x l r) = Node <$> f x <*> traverse f l <*> traverse f r
