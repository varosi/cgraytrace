module Raytracer( raytrace, Entity, Scene, Sensor(..), Camera(..), Coord3(..) ) where

import Codec.Picture.Types (Image(..), PixelRGB8(..), generateImage)
import Linear.V2
import Linear.V3

-- http://hackage.haskell.org/package/linear

type Entity = Int
type Scene  = [Entity]

newtype ScreenSpace a = SS     (V2 a)
newtype Coord3 r      = Coord3 (V3 r)

newtype Sensor = Sensor (Int, Int) -- width, height
data Camera r  = Camera Sensor (Coord3 r)

-- Image space to screen space
toScreenSpace :: (Integral i, Fractional f) => Sensor -> i -> i -> ScreenSpace f
toScreenSpace (Sensor (width, height)) x y = SS $ V2 sx sy where
        sx = fromIntegral x / (fromIntegral width  - 1)
        sy = fromIntegral y / (fromIntegral height - 1)

-- Single sample
sample :: Scene -> ScreenSpace Float -> PixelRGB8
sample _ (SS (V2 x y)) = PixelRGB8 a 0 0 where a = if x < 0.5 then 0 else 0xff

-- Ray-trace whole image viewed by camera
raytrace :: Fractional r => Scene -> Camera r -> Image PixelRGB8
raytrace scene (Camera sensor@(Sensor (width, height)) _) = generateImage imageSample width height where
    imageSample x y = sample scene $ toScreenSpace sensor x y