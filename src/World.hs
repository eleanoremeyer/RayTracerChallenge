module World (World (..), viewTransform) where

import Control.Lens hiding (transform)
import Data.Function
import Data.List (sortBy)
import Ray
import Object hiding (intersect)
import Light
import Types
import Object.Sphere
import Material
import Vectors
import Matrix

data World = World { worldObjects :: ![Obj], worldLights :: ![Light] }

instance Default World where
  def = World [toObj mat unitSphere, toObj def (transform (mkScaling $ V3 0.5 0.5 0.5) unitSphere)] [def]
    where
      mat = def { materialColour = V3 0.8 1.0 0.6
                , materialDiffuse = 0.7
                , materialSpecular = 0.2
                }

viewTransform :: V4 Number -> V4 Number -> V4 Number -> M44 Number
viewTransform from to up =
  let forward = normalize $ to - from in
  let upn = normalize up in
  let left = cross forward upn in
  let trueUp = cross left forward in
  let orientation =
          V4
            (V4 (left^._x)     (left^._y)     (left^._z)     0)
            (V4 (trueUp^._x)   (trueUp^._y)   (trueUp^._z)   0)
            (V4 (-forward^._x) (-forward^._y) (-forward^._z) 0)
            (V4 0              0              0              1) in
  orientation !*! mkTranslation (V3 (-from^._x) (-from^._y) (-from^._z))
