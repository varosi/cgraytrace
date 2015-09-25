module Raytracer( raytrace, Entity, Scene, Sensor(..), Camera(..) ) where

import Camera
import Geometry
import Codec.Picture.Types (Image(..), PixelRGB8(..), generateImage)
import Scene
import Math
import Light

mapEnergy :: Energy -> PixelRGB8
mapEnergy (Energy (r, g, b)) = PixelRGB8 (f2w r) (f2w g) (f2w b) where
        f2w f = truncate (f * 255)

depthMap :: Intersection -> Energy
depthMap Environment = envEnergy
depthMap (Hit t _ _) = Energy (a, a, a) where a = t / 1000

traceRay :: Scene -> Ray -> Intersection
traceRay scene ray = foldl closest Environment . map (intersect ray) . scEntities $ scene where
        closest x0 Environment                  = x0
        closest Environment x                   = x
        closest x0@(Hit t0 _ _) x@(Hit t _ _)   = if t < t0 then x else x0

rayCast :: Scene -> Ray -> Energy
rayCast scene = depthMap . traceRay scene

pathTrace :: Scene -> Ray -> Energy
pathTrace scene cameraRay' = result firstHit where
    firstHit            = traceRay scene cameraRay'

    result Environment  = envEnergy
    result hit          = case traceShadow hit of
                            Environment -> eval light -- shadow ray is traced to the light - 100% diffuse reflection
                            Hit {}      -> envEnergy  -- light is shadowed by some object

    light               = head . scLights $ scene
    shadow hit          = shadowRay light . isectPoint $ hit
    traceShadow         = traceRay scene . shadow

imageSample :: Camera cam => Scene -> cam -> UnitSpace -> Energy
imageSample scene camera = pathTrace scene . cameraRay camera

-- Ray-trace whole image viewed by camera
raytrace :: Camera cam => Scene -> cam -> Image PixelRGB8
raytrace scene camera = generateImage pixelColor width height where
    pixelColor x y                  = mapEnergy . imageSample scene camera $ toScreenSpace sensor x y
    sensor@(Sensor (width, height)) = cameraSensor camera
