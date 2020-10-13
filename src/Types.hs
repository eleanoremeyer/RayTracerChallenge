module Types (Number, Colour, Point, Vector, Transformable (..), Default (..)) where

import Linear (V3, V4, M44)

type Number = Double

type Colour = V3 Number

type Point = V4 Number
type Vector = V4 Number

class Transformable a where
  transform :: M44 Number -> a -> a

class Default a where
  def :: a
