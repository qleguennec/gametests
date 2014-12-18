module Util where

tupleAp :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
tupleAp f (a, a') (b, b') = (f a b, f a' b')
