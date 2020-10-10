{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}

module Object (Intersection (..), Object (..), intersect, hit, reflect) where

import Types (Number)
import Material
import Data.Function (on)
import Data.List (sortBy, find)
import Ray (Ray)
import Vectors

data Intersection = forall a. Object a =>
  Intersection { intersectionTime :: !Number, intersectionObject :: !a}
deriving instance Show Intersection

class Show a => Object a where
  intersection :: Ray -> a -> [Number]
  normalAt :: V4 Number -> a -> V4 Number
  material :: a -> Material

intersect :: Object a => Ray -> a -> [Intersection]
intersect ray a = (`Intersection` a) <$> intersection ray a

hit :: [Intersection] -> Maybe Intersection
hit = find ((0 <) . intersectionTime) . sortBy (compare `on` intersectionTime)

reflect :: V4 Number -> V4 Number -> V4 Number
reflect incoming normal = incoming - normal ^* (2 * dot incoming normal)
