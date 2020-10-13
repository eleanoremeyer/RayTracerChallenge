{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Object (Intersection (..), HasTime (..), HasObject (..)
              , Obj (..), HasMaterial (..), HasNormalAt (..)
              , ToObj (..), intersect, hit, reflect) where

import Control.Lens
import Matrix
import Types (Number, Transformable (..))
import Material
import Data.Function (on)
import Data.List (sortBy, find)
import Ray (Ray)
import Vectors

data Obj = Obj {
  _objIntersection :: Ray -> [Number],
  _objNormalAt :: V4 Number -> V4 Number,
  _objMaterial :: Material,
  _objTransform :: M44 Number -> Obj
}
$(makeFields ''Obj)

class ToObj a where
  toObj :: Material -> a -> Obj

instance Transformable Obj where
  transform = flip _objTransform

data Intersection =
  Intersection { _intersectionTime :: !Number, _intersectionObject :: !Obj}

$(makeFields ''Intersection)

intersect :: Ray -> Obj -> [Intersection]
intersect ray obj@Obj {..} = (`Intersection` obj) <$> (obj^.intersection) ray

hit :: [Intersection] -> Maybe Intersection
hit = find ((0 <) . view time) . sortBy (compare `on` view time)

reflect :: V4 Number -> V4 Number -> V4 Number
reflect incoming normal = incoming - normal ^* (2 * dot incoming normal)


