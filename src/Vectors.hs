module Vectors
  ( (^+^), (^-^), negated, (^*), (*^), (^/), norm, normalize, dot, cross, floatingEquality, point, vector
  , V3 (..), V4 (..), _x, _y, _z, _w) where

import Linear hiding (point, vector, cross)
import qualified Linear as L (point, vector, cross)

floatingEquality :: V4 Double -> V4 Double -> Bool
floatingEquality a b = nearZero (a-b)

point :: Num a => a -> a -> a -> V4 a
point a b c = L.point $ V3 a b c

vector :: Num a => a -> a -> a -> V4 a
vector a b c = L.vector $ V3 a b c

-- three dimensional cross product embedded
cross :: Num a => V4 a -> V4 a -> V4 a
cross (V4 x1 y1 z1 _) (V4 x2 y2 z2 _) = L.vector $ V3 x1 y1 z1 `L.cross` V3 x2 y2 z2
