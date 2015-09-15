module Raytracer( raytrace, Entity, Scene, Sensor(..), Camera(..), Coord3(..) ) where

import Codec.Picture.Types (Image(..), PixelRGB8(..), generateImage)
import Linear.V2
import Linear.V3
import Data.Word8

-- http://hackage.haskell.org/package/linear

type Entity = Int
type Scene  = [Entity]

newtype ScreenSpace a = SS     (V2 a)
newtype Coord3 r      = Coord3 (V3 r)

newtype Ray r         = Ray (Coord3 r, V3 r) -- position & direction

newtype Energy        = Energy (Float, Float, Float) -- R, G, B components of energy that we sense

newtype Sensor = Sensor (Int, Int)           -- width, height
data Camera r  = Camera Sensor (Coord3 r)

cameraRay :: Camera r -> ScreenSpace r -> Ray r
cameraRay = undefined

mapEnergy :: Energy -> PixelRGB8
mapEnergy (Energy (r, g, b)) = PixelRGB8 (f2w r) (f2w g) (f2w b) where
        f2w f = truncate (f * 255.0)

-- Image space to screen space
toScreenSpace :: (Integral i, Fractional f) => Sensor -> i -> i -> ScreenSpace f
toScreenSpace (Sensor (width, height)) x y = SS $ V2 sx sy where
        sx = fromIntegral x / (fromIntegral width  - 1)
        sy = fromIntegral y / (fromIntegral height - 1)

-- Single sample
sample :: Scene -> ScreenSpace Float -> Energy
sample _ (SS (V2 x y)) = Energy (a, 0.0, 0.0) where a = if x < 0.5 then 0.0 else 1.0

-- Ray-trace whole image viewed by camera
raytrace :: Fractional r => Scene -> Camera r -> Image PixelRGB8
raytrace scene (Camera sensor@(Sensor (width, height)) _) = generateImage imageSample width height where
    imageSample x y = mapEnergy . sample scene $ toScreenSpace sensor x y