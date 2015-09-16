module Raytracer( raytrace, Entity, Scene, Sensor(..), Camera(..), Coord3(..) ) where

import Math
import Camera
import Codec.Picture.Types (Image(..), PixelRGB8(..), generateImage)
import Linear.V2
import Linear.V3
import Data.Word8

-- http://hackage.haskell.org/package/linear

type Entity = Int
type Scene  = [Entity]

newtype Energy = Energy (Float, Float, Float)    -- R, G, B components of energy that we sense

mapEnergy :: Energy -> PixelRGB8
mapEnergy (Energy (r, g, b)) = PixelRGB8 (f2w r) (f2w g) (f2w b) where
        f2w f = truncate (f * 255.0)

-- Single sample
sample :: Scene -> ScreenSpace -> Energy
sample _ (SS (V2 x y)) = Energy (a, 0.0, 0.0) where a = if x < 0.5 then 0.0 else 1.0

-- Ray-trace whole image viewed by camera
raytrace :: Scene -> Camera -> Image PixelRGB8
raytrace scene camera = generateImage imageSample width height where
    imageSample x y                 = mapEnergy . sample scene $ toScreenSpace sensor x y
    sensor@(Sensor (width, height)) = camSensor camera
