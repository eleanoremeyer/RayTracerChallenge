module Main where

import Lib
import GUI.Main
import Render
import Codec.Picture

main :: IO ()
main = savePngImage "out.png" $ ImageRGB8 renderRaySphereIntersections
