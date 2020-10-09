module Vectors ((^+^), (^-^), (^*^), negated, (*^), (^/), norm, normalize, dot, cross, floatingEquality, point, vector, V3 (..), V4 (..)) where

import Linear

floatingEquality :: V4 Double -> V4 Double -> Bool
floatingEquality a b = nearZero (a-b)

(^*^) :: (Num a, Additive f) => f a -> f a -> f a
(^*^) = liftU2 (*)
{-# INLINE (^*^) #-}
