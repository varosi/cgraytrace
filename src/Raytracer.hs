module Raytracer( raytrace, Geometry, Scene, Sensor(..), Camera(..), traceRay, pathTrace ) where

import Camera
import Geometry
import Codec.Picture.Types (Image(..), PixelRGB8(..), generateImage)
import Scene
import Math
import Light
import Linear
import Material
import BRDF

mapEnergy :: Energy -> PixelRGB8
mapEnergy (Energy (V3 r g b)) = PixelRGB8 (f2w r) (f2w g) (f2w b) where
        f2w f = truncate (f * 255)

--depthMap :: Intersection a -> Energy
--depthMap Environment = envEnergy
--depthMap (Hit t _ _) = Energy $ V3 a a a where a = t / 1000

traceRay :: Scene -> Maybe Entity -> Ray -> Intersection Entity
traceRay scene toSkip ray@(Ray (origin, normal)) = foldl closest Environment . map (intersect ray) . (skip toSkip) . scEntities $ scene where
        closest x0 Environment  = x0
        closest Environment x   = x
        closest x0@(Hit t0 _ _ entity0) x@(Hit t _ _ entity)  = if (t < t0) && (entity /= entity0) then x else x0

        skip Nothing xs  = xs
        skip (Just e) xs = filter (/= e) xs

--rayCast :: Scene -> Ray -> Energy
--rayCast scene = depthMap . traceRay scene

pathTrace :: Scene -> Ray -> Energy
pathTrace scene cameraRay' = bounce' geomHit where
    geomHit = traceRay scene Nothing cameraRay'

    bounce' Environment             = envEnergy
    bounce' hit@(Hit _ ipoint _ entity') = reflectedLight where
        Mat brdf            = enMaterial entity'
        light               = head . scLights $ scene                        -- Single light support currently
        shadowRay'          = shadowRay light ipoint
        Ray (_, dir2light)  = shadowRay'

        reflectedLight = case traceRay scene (Just entity') shadowRay' of
            Environment -> evalBRDF brdf hit dir2light . eval $ light -- shadow ray is traced to the light - 100% diffuse reflection
            Hit {}      -> envEnergy                                  -- light is shadowed by some object

imageSample :: Camera cam => Scene -> cam -> UnitSpace -> Energy
imageSample scene camera = pathTrace scene . cameraRay camera

-- Ray-trace whole image viewed by camera
raytrace :: Camera cam => Scene -> cam -> Image PixelRGB8
raytrace scene camera = generateImage pixelColor width height where
    pixelColor x y                     = mapEnergy . imageSample scene camera $ toScreenSpace sensor x y
    sensor@(Sensor (width, height, _)) = cameraSensor camera
