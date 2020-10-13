{-# LANGUAGE RecordWildCards #-}

module Object.Sphere (unitSphere) where

import Control.Lens hiding (transform)

import Types (Transformable (..), Number)
import Object (ToObj (..), Obj (..))
import Vectors
import Matrix
import Ray

data Sphere = Sphere { trans :: M44 Number, invTrans :: M44 Number}

intersection :: Sphere -> Ray -> [Number]
intersection Sphere {..} ray =
  let Ray {..} = transformRay invTrans ray in
  let sphereToRay  = rayOrigin  - point (V3 0 0 0)
      a            = dot rayDirection rayDirection
      b            = 2 * dot rayDirection sphereToRay
      c            = dot sphereToRay sphereToRay - 1
      discriminant = b^(2::Int) - 4 * a * c
  in
  if discriminant < 0 then
    []
  else
    let sqrtDiscriminant = sqrt discriminant in
    [(-b - sqrtDiscriminant)/(2*a), (-b + sqrtDiscriminant)/(2*a)]

instance Transformable Sphere where
  transform trans' Sphere {..} = Sphere (trans' !*! trans) (invTrans !*! inv44 trans')

normalAt :: Sphere -> V4 Number -> V4 Number
normalAt Sphere {..} worldPoint =
  let objectPoint = invTrans !* worldPoint in
  let objectNormal = objectPoint - point (V3 0 0 0) in
  let worldNormal = transpose invTrans !* objectNormal in
  normalize $ worldNormal & _w .~ 0


unitSphere :: Sphere
unitSphere = Sphere identity identity

instance ToObj Sphere where
  toObj mat sphere = Obj { _objIntersection = intersection sphere
                         , _objNormalAt = normalAt sphere
                         , _objMaterial = mat
                         , _objTransform = toObj mat . flip transform sphere
                         }
