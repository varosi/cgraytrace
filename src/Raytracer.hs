module Raytracer( raytrace, Entity, Scene, Sensor(..), Camera(..), Coord3(..) ) where

import Math
import Camera
import Geometry
import Codec.Picture.Types (Image(..), PixelRGB8(..), generateImage)
import Linear.V2
import Linear.V3
import Data.Word8
import Scene

newtype Energy = Energy (Float, Float, Float)    -- R, G, B components of energy that we sense

mapEnergy :: Energy -> PixelRGB8
mapEnergy (Energy (r, g, b)) = PixelRGB8 (f2w r) (f2w g) (f2w b) where
        f2w f = truncate (f * 255.0)

-- Single sample
sample :: Camera cam => cam -> Scene -> ScreenSpace -> Energy
sample camera scene pos@(SS (V2 x y)) = Energy (a, 0, 0) where
        a       = if or isect then 1 else 0
        ray     = cameraRay camera pos
        isect   = map (isIntersected.(intersect ray)) scene     :: [Bool]

-- Ray-trace whole image viewed by camera
raytrace :: Camera cam => Scene -> cam -> Image PixelRGB8
raytrace scene camera = generateImage imageSample width height where
    imageSample x y                 = mapEnergy . sample camera scene $ toScreenSpace sensor x y
    sensor@(Sensor (width, height)) = cameraSensor camera
