{-# LANGUAGE RecordWildCards #-}

module Object.Sphere (Sphere, unitSphere, transformSphere) where

import Object
import Vectors
import Matrix
import Types (Number)
import Ray

newtype Sphere = Sphere (M44 Number) deriving Show

instance Object Sphere where
  intersection ray (Sphere trans) =
    let Ray {..} = transformRay (inv44 trans) ray in
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

unitSphere :: Sphere
unitSphere = Sphere identity

transformSphere :: M44 Number -> Sphere -> Sphere
transformSphere trans' (Sphere trans) = Sphere (trans' !*! trans)
