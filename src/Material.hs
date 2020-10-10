module Material (Material (..), defaultMaterial) where

import Types (Number, Colour)
import Vectors

data Material = Material { materialColour    :: !Colour
                         , materialAmbient   :: !Number
                         , materialDiffuse   :: !Number
                         , materialSpecular  :: !Number
                         , materialShininess :: !Number } deriving Show

defaultMaterial :: Material
defaultMaterial = Material { materialColour = V3 1 0.2 1
                           , materialAmbient = 0.1
                           , materialDiffuse = 0.9
                           , materialSpecular = 0.9
                           , materialShininess = 200.0 }
