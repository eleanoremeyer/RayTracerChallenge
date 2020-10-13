module Light (Light (..)) where

import Vectors
import Object
import Types
import Material

data Light = PointLight { lightPos :: !(V4 Number), lightIntensity :: !Colour }

instance Default Light where
  def = PointLight (point (-10) 10 (-10)) (V3 1 1 1)

