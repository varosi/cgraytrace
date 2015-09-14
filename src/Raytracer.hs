module Raytracer( raytrace, Entity(..), Scene(..), Sensor(..), Camera(..) ) where

import Codec.Picture.Types (Image(..), PixelRGB8)
import Data.Vector.Storable (generate)
import Linear.V3

-- http://hackage.haskell.org/package/linear

type Entity = Int
type Scene  = [Entity]

newtype Sensor = Sensor (Int, Int) -- width, height
newtype Camera = Camera Sensor

raytrace :: Scene -> Camera -> Image PixelRGB8
raytrace _ (Camera (Sensor (width, height))) = Image width height pixels where
    pixels = generate (width*height*3) (\i -> if i `mod` (16*3) == 0 then 0 else 0xFF)