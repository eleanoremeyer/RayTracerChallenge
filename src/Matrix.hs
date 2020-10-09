{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Matrix ( mkTranslation, mkScaling, mkRotationAroundX, mkRotationAroundY, mkRotationAroundZ
              , Transform, runTransform, translate, scale, rotateX, rotateY, rotateZ, shear
              , M44, identity, (!*!), inv44)
  where

import Control.Monad.State.Strict
import Linear
import Types (Number)

mkTranslation :: V3 Number -> M44 Number
mkTranslation (V3 x y z) = V4 (V4 1 0 0 x) (V4 0 1 0 y) (V4 0 0 1 z) (V4 0 0 0 1)

mkScaling :: V3 Number -> M44 Number
mkScaling (V3 x y z) = V4 (V4 x 0 0 0) (V4 0 y 0 0) (V4 0 0 z 0) (V4 0 0 0 1)


-- The Angle is in Radians

mkRotationAroundX :: Number -> M44 Number
mkRotationAroundX r =
  let (cosr, sinr) = (cos r, sin r) in
  V4 (V4 1 0 0 0) (V4 0 cosr (-sinr) 0) (V4 0 sinr cosr 0) (V4 0 0 0 1)

mkRotationAroundY :: Number -> M44 Number
mkRotationAroundY r =
  let (cosr, sinr) = (cos r, sin r) in
  V4 (V4 cosr 0 sinr 0) (V4 0 1 0 0) (V4 (-sinr) 0 cosr 0) (V4 0 0 0 1)

mkRotationAroundZ :: Number -> M44 Number
mkRotationAroundZ r =
  let (cosr, sinr) = (cos r, sin r) in
  V4 (V4 cosr (-sinr) 0 0) (V4 sinr cosr 0 0) (V4 0 0 1 0) (V4 0 0 0 1)

mkShearing :: V3 (V2 Number) -> M44 Number
mkShearing (V3 (V2 xy xz) (V2 yx yz) (V2 zx zy)) =
  V4 (V4 1 xy xz 0) (V4 yx 1 yz 0) (V4 zx zy 1 0) (V4 0 0 0 1)

newtype Transform a = Transform (State (M44 Number) a) deriving (Functor, Applicative, Monad, MonadState (M44 Number))

runTransform :: Transform a -> M44 Number
runTransform (Transform trans) = execState trans identity

translate :: V3 Number -> Transform ()
translate n = modify' (mkTranslation n !*!)

scale :: V3 Number -> Transform ()
scale n = modify' (mkScaling n !*!)

rotateX :: Number -> Transform ()
rotateX n = modify' (mkRotationAroundX n !*!)

rotateY :: Number -> Transform ()
rotateY n = modify' (mkRotationAroundY n !*!)

rotateZ :: Number -> Transform ()
rotateZ n = modify' (mkRotationAroundZ n !*!)

shear :: V3 (V2 Number) -> Transform ()
shear n = modify' (mkShearing n !*!)
