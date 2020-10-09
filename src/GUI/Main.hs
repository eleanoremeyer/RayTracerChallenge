{-# LANGUAGE RecordWildCards #-}

module GUI.Main where

import Data.Maybe (fromJust)
import Graphics.Gloss
import Graphics.Gloss.Juicy
import Codec.Picture

showImage :: Image PixelRGB8 -> IO ()
showImage img@Image {..} = display (InWindow "Gloss: RayTracer" (imageWidth,imageHeight) (500,500)) black $ fromImageRGB8 img
