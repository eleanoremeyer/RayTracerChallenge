{-# LANGUAGE RecordWildCards #-}
module Ray (Ray (..), position, transformRay) where

import Linear
import Types (Number)

data Ray = Ray { rayOrigin :: V4 Number, rayDirection :: V4 Number} deriving Show

position :: Ray -> Number -> V4 Number
position Ray {..} t = rayOrigin ^+^ t *^ rayDirection

transformRay :: M44 Number -> Ray -> Ray
transformRay transform Ray {..} = Ray (transform !* rayOrigin) (transform !* rayDirection)
