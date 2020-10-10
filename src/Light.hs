{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Light (Light (..), lighting) where

import Vectors
import Object
import Types (Number, Colour, Point, Vector)
import Material

data Light = PointLight { lightPos :: !(V4 Number), lightIntensity :: !Colour }

-- material, illuminated point, light soruce, eye vector, normal vector
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
