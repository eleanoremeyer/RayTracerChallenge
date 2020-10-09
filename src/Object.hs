{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}

module Object (Object (..), intersect, hit) where

import Types (Number)
import Data.Function (on)
import Data.List (sortBy, find)
import Control.Lens
import Ray (Ray)
import Linear (V4)

data Intersection = forall a. Object a =>
  Intersection { intersectionTime :: Number, intersectionObject :: a}
deriving instance Show Intersection

class Show a => Object a where
  intersection :: Ray -> a -> [Number]

intersect :: Object a => Ray -> a -> [Intersection]
intersect ray a = (`Intersection` a) <$> intersection ray a

hit :: [Intersection] -> Maybe Intersection
hit = find ((0 <) . intersectionTime) . sortBy (compare `on` intersectionTime)
