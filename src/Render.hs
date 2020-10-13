{-# LANGUAGE RecordWildCards #-}
module Render ( renderDrawingCanvas
              , renderRaySphereIntersections
              , renderMatrixTransformations
              , renderLightAndShading) where

import Types
import Ray
import Matrix
import Light
import Object
import Control.Lens hiding (transform)
import Data.Maybe (catMaybes)
import Codec.Picture
import Codec.Picture.Types
import Linear
import Control.Monad.State.Strict
import Control.Monad.ST.Strict
import Object.Sphere

data Projectile = Projectile { pos :: V3 Number, velocity :: V3 Number }

-- Draw a list of (red) pixels on a black canvas
imgWithPixels :: Int -> Int -> [(Int, Int)] -> Image PixelRGB8
imgWithPixels width height pixels = runST $ do
  pic <- createMutableImage width height (PixelRGB8 0 0 0)
  mapM_ (\(x,y) -> writePixel pic x y (PixelRGB8 255 0 0)) pixels
  freezeImage pic

genImageFlipped x y f = generateImage f x y

-- Chapter 2
renderDrawingCanvas :: Image PixelRGB8
renderDrawingCanvas = imgWithPixels width height $
                        (_2 %~ (height -)) <$> catMaybes (evalState (replicateM 10000 update) projectile)
  where
    width = 900
    height = 550
    gravity = V3 0 (-0.1) 0 :: V3 Number
    wind = V3 (-0.01) 0 0 :: V3 Number
    projectile = Projectile (V3 0 1 0) (11.25 *^ normalize (V3 1 1.8 0))
    timestep = 0.1

    update :: State Projectile (Maybe (Int, Int))
    update = do
      Projectile {..} <- get
      put $ Projectile (pos + timestep * velocity) (velocity + timestep * (gravity + wind))
      let (x, y) = (round (pos^._x), round (pos^._y))
      if 0 < x && x <= width && 0 < y && y <= height then
        pure $ Just (x,y)
      else
        pure Nothing


-- Chapter 4
renderMatrixTransformations :: Image PixelRGB8
renderMatrixTransformations =
  imgWithPixels width height $
    [(round $ pos^._x, round $ pos^._y) | i<-[0..11], let pos = pixel (i*pi/6)]
  where
    width = 60
    height = 60
    distanceToOrigin = 15
    trans degree = runTransform $ do
      -- start at noon
      translate $ V3 0 (-distanceToOrigin) 0
      rotateZ degree
      -- move to origin
      translate $ V3 (fromIntegral $ width `div` 2) (fromIntegral $ height `div` 2) 0
    pixel degree = trans degree !* point (V3 0 0 0)


-- Chapter 5
renderRaySphereIntersections :: Image PixelRGB8
renderRaySphereIntersections =
  genImageFlipped canvasPixels canvasPixels $ \x y ->
    let worldX = pixelSize*fromIntegral x - halfWallSize in
    let worldY = halfWallSize - pixelSize * fromIntegral y in
    let ray = Ray rayOrig (point (V3 worldX  worldY  wallZ) - rayOrig) in
    maybe (PixelRGB8 0 0 0) (const $ PixelRGB8 255 0 0) $ hit $ intersect ray $ toObj def $
      flip transform unitSphere $
        runTransform $ do
          shear $ V3 (V2 1 0) (V2 0 0) (V2 0 0)
          scale $ V3 0.5 1 1

  where
    rayOrig = point $ V3 0 0 (-5)
    wallZ = 10
    wallSize = 7
    canvasPixels = 100 :: Int
    pixelSize = wallSize/fromIntegral canvasPixels
    halfWallSize = wallSize/2


-- Chapter 6
renderLightAndShading :: Image PixelRGB8
renderLightAndShading =
  genImageFlipped canvasPixels canvasPixels $ \x y ->
    let worldX = pixelSize*fromIntegral x - halfWallSize in
    let worldY = halfWallSize - pixelSize * fromIntegral y in
    let ray = Ray rayOrig (normalize $ point (V3 worldX  worldY  wallZ) - rayOrig) in

    renderMaybeIntersection ray $ \inter ->
      let point = position ray (inter^.time) in
      let normal = (inter^.object.normalAt) point in
      let eye = -(rayDirection ray) in
      let color = lighting (inter^.object.material) light point eye normal in
      PixelRGB8
        (computeColorValue $ color^._x)
        (computeColorValue $ color^._y)
        (computeColorValue $ color^._z)

  where
    computeColorValue :: Number -> Pixel8
    computeColorValue i
      | i<=0       = minBound
      | i>=1       = maxBound
      | otherwise  = round (fromIntegral (maxBound :: Pixel8) * i)

    lightPosition = point $ V3 (-10) 10 (-10)
    lightColour = V3 1 1 1
    light = PointLight lightPosition lightColour
    rayOrig = point $ V3 0 0 (-5)
    wallZ = 10
    wallSize = 7
    canvasPixels = 256 :: Int
    pixelSize = wallSize/fromIntegral canvasPixels
    halfWallSize = wallSize/2
    sphere = toObj def $ flip transform unitSphere $
      runTransform $ do
        pure ()
        -- shear $ V3 (V2 1 0) (V2 0 0) (V2 0 0)
        -- scale $ V3 0.5 1 1
    renderMaybeIntersection ray handler = maybe (PixelRGB8 0 0 0) handler $ hit $ intersect ray sphere
