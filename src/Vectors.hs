module Vectors
  ( (^+^), (^-^), negated, (^*), (*^), (^/), norm, normalize, dot, cross, floatingEquality, point, vector
  , V3 (..), V4 (..), _x, _y, _z, _w) where

import Linear

floatingEquality :: V4 Double -> V4 Double -> Bool
floatingEquality a b = nearZero (a-b)

