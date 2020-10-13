{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Object ( Obj (..), HasIntersection (..), HasMaterial (..), HasNormalAt (..)
              , ToObj (..)) where

import Control.Lens
import Matrix
import Types (Number, Transformable (..))
import Material
import Data.Function (on)
import Data.List (sortBy, find)
import Ray (Ray)
import Vectors

data Obj = Obj {
  _objIntersection :: !(Ray -> [Number]),
  _objNormalAt :: !(V4 Number -> V4 Number),
  _objMaterial :: !Material,
  _objTransform :: !(M44 Number -> Obj)
}
$(makeFields ''Obj)

instance Show Obj where
  show = const "Object"

class ToObj a where
  toObj :: Material -> a -> Obj

instance Transformable Obj where
  transform = flip _objTransform

