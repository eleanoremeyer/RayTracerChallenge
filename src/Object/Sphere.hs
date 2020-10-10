{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Object.Sphere (Sphere, unitSphere, transformSphere) where

import Control.Lens

import Material
import Object
import Vectors
import Matrix
import Types (Number)
import Ray

data Sphere = Sphere { _sphereTrans :: !(M44 Number), _sphereInvTrans :: !(M44 Number), _sphereMaterial :: !Material }
  deriving Show

$(makeLenses ''Sphere)

instance Object Sphere where
  intersection ray (Sphere trans invTrans _) =
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

  normalAt worldPoint (Sphere trans invTrans _) =
    let objectPoint = invTrans !* worldPoint in
    let objectNormal = objectPoint - point (V3 0 0 0) in
    let worldNormal = transpose invTrans !* objectNormal in
    normalize $ worldNormal & _w .~ 0

  material = view sphereMaterial

unitSphereWithMaterial :: Material -> Sphere
unitSphereWithMaterial = Sphere identity identity

unitSphere :: Sphere
unitSphere = unitSphereWithMaterial defaultMaterial

transformSphere :: M44 Number -> Sphere -> Sphere
transformSphere trans' = (sphereInvTrans %~ (!*! inv44 trans') ) . (sphereTrans %~ (trans' !*!))
