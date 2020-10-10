module Types (Number, Colour, Point, Vector) where

import Linear (V3, V4)

type Number = Double

type Colour = V3 Number

type Point = V4 Number
type Vector = V4 Number
