module Raytracer( raytrace, Entity, Scene, Sensor(..), Camera(..) ) where

import Camera
import Geometry
import Codec.Picture.Types (Image(..), PixelRGB8(..), generateImage)
import Scene

newtype Energy = Energy (Float, Float, Float)    -- R, G, B components of energy that we sense

mapEnergy :: Energy -> PixelRGB8
mapEnergy (Energy (r, g, b)) = PixelRGB8 (f2w r) (f2w g) (f2w b) where
        f2w f = truncate (f * 255)

depthMap :: [Intersection] -> Float
depthMap = foldl min maxBound . map energy where
    energy Environment = 0.0
    energy (Hit t)     = (t / 1000)

-- Single sample
sample :: Camera cam => cam -> Scene -> ScreenSpace -> Energy
sample camera scene pos = Energy (a, a, a) where
        a       = depthMap . map (intersect ray) $ scene
        ray     = cameraRay camera pos

-- Ray-trace whole image viewed by camera
raytrace :: Camera cam => Scene -> cam -> Image PixelRGB8
raytrace scene camera = generateImage imageSample width height where
    imageSample x y                 = mapEnergy . sample camera scene $ toScreenSpace sensor x y
    sensor@(Sensor (width, height)) = cameraSensor camera
