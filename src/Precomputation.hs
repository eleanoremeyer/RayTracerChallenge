{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
module Precomputation ( Intersection (..), Intersectionable (..), HasTime (..), HasObject (..)
                      , hit, lighting, prepareComputations, shadeHit, colorAt) where

import Control.Lens
import World
import Types
import Light
import Material
import Data.Function (on)
import Data.List (find, sortBy)
import Object
import Ray
import Vectors hiding (point)

data Intersection =
  Intersection { _intersectionTime :: !Number, _intersectionObject :: !Obj}

$(makeFields ''Intersection)

instance Show Intersection where
  show = show . view time

class Intersectionable a where
  intersect :: Ray -> a -> [Intersection]


sortByTime :: [Intersection] -> [Intersection]
sortByTime = sortBy (compare `on` view time)

instance Intersectionable Obj where
  intersect ray obj@Obj {..} = sortByTime $! (`Intersection` obj) <$> (obj^.intersection) ray

instance Intersectionable World where
  intersect ray World {..} = sortByTime $! worldObjects >>= intersect ray

hit :: [Intersection] -> Maybe Intersection
hit = find ((0 <) . view time)

reflect :: V4 Number -> V4 Number -> V4 Number
reflect incoming normal = incoming - normal ^* (2 * dot incoming normal)

data Precomputation = Precomputation { _precomputationObject :: !Obj
                                     , _precomputationPoint :: !(V4 Number)
                                     , _precomputationEyeV :: !(V4 Number)
                                     , _precomputationNormalV :: !(V4 Number)
                                     , _precomputationsInside :: !Bool
                                     } deriving Show

$(makeFields ''Precomputation)

prepareComputations :: Intersection -> Ray -> Precomputation
prepareComputations int r@Ray {..} =
  let interPoint =  position r $ int^.time in
  let normalV = (int^.object.normalAt) interPoint in
  let eyeV = -rayDirection in
  let inside = dot normalV eyeV < 0 in
  Precomputation { _precomputationObject = int^.object
                 , _precomputationPoint = interPoint
                 , _precomputationEyeV = eyeV
                 , _precomputationNormalV = if inside then -normalV else normalV
                 , _precomputationsInside = inside
                 }


shadeHit :: World -> Precomputation -> Colour
shadeHit World {..} prec = sum $! map computeLightSource worldLights
  where
    computeLightSource lightSource = lighting (prec^.object.material) lightSource (prec^.point) (prec^.eyeV) (prec^.normalV)


colorAt :: World -> Ray -> Colour
colorAt world ray = maybe (V3 0 0 0) (\inter -> shadeHit world (prepareComputations inter ray)) (hit $ ray `intersect` world)


-- material, illuminated point, light source, eye vector, normal vector
lighting :: Material -> Light -> Point -> Vector -> Vector -> Colour
lighting Material {..} PointLight {..} illuminatedPoint eyeVec normalVec =
  let effectiveColour = materialColour * lightIntensity in

  -- direction to the light source
  let lightVec = normalize $ lightPos - illuminatedPoint in

  --- ambient contribution
  let ambient = effectiveColour ^* materialAmbient in

  -- compute the cosine of the angle between the light and normal vector
  -- if it is <0 then the light is on the other side of the surface
  let lightDotNormal = dot lightVec normalVec in

  let (diffuse, specular) =
        if lightDotNormal < 0 then
          let black = V3 0 0 0 in (black, black)
        else
          -- reflectDotEye computes the cosine of the angle between the light and normal vector
          -- if it is <0 then the light reflects away from the eye
          let reflectVec = reflect (-lightVec) normalVec in
          let reflectDotEye = dot reflectVec eyeVec in
          (effectiveColour ^* materialDiffuse ^* lightDotNormal, ) $
            -- compute specular
            if reflectDotEye <= 0 then V3 0 0 0 else
              let factor = reflectDotEye**materialShininess in
              lightIntensity ^* materialSpecular ^* factor
  in
  -- specular
  ambient + diffuse + specular
