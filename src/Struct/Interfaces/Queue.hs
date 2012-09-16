module Struct.Interfaces.Queue where

class Queue q where
    head :: q a -> a
    tail :: q a -> q a
    snoc :: a -> q a -> q a
    isEmpty :: q a -> Bool

class Queue q => Deque q where
    init :: q a -> q a
    last :: q a -> a
    cons :: a -> q a -> q a

