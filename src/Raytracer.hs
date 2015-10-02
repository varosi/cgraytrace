module Raytracer( raytrace, Geometry, Scene, Sensor(..), Camera(..), traceRay, pathTrace ) where

import Camera
import Geometry
import Codec.Picture.Types (Image(..), PixelRGB8(..), generateFoldImage)
import Scene
import Math
import Light
import Linear
import Material
import BRDF
import System.Random (RandomGen(..))

-- rayCast (with depth) or pathTrace
method :: RandomGen gen => gen -> Scene -> Ray -> (Energy, gen)
method = pathTrace

mapEnergy :: Energy -> PixelRGB8
mapEnergy (Energy (V3 r g b)) = PixelRGB8 (f2w r) (f2w g) (f2w b) where
        f2w f = truncate (f * 255)

depthMap :: Intersection a -> Energy
depthMap Environment = envEnergy
depthMap (Hit t _ _ _) = Energy $ V3 a a a where a = t / 100

traceRay :: Scene -> Maybe Entity -> Ray -> Intersection Entity
traceRay scene toSkip ray = foldl closest Environment . map (intersect ray) . skip toSkip . scEntities $ scene where
        closest x0 Environment                                = x0
        closest Environment x@(Hit t _ _ _)                   = if t >= 0 then x else Environment
        closest x0@(Hit t0 _ _ entity0) x@(Hit t _ _ entity)  = if (entity /= entity0) && (t >= 0) && (t < t0) then x else x0

        skip Nothing xs  = xs
        skip (Just e) xs = filter (/= e) xs

rayCast :: Scene -> Ray -> Energy
rayCast scene = depthMap . traceRay scene Nothing

pathTrace :: RandomGen gen => gen -> Scene -> Ray -> (Energy, gen)
pathTrace g0 scene cameraRay' = bounce' g0 geomHit where
    geomHit = traceRay scene Nothing cameraRay'

    bounce' g Environment                  = (envEnergy, g)
    bounce' g hit@(Hit _ ipoint _ entity') = (reflectedLight, g') where
        Mat brdf            = enMaterial entity'
        light               = head . scLights $ scene                        -- Single light support currently
        (shadowRay', g')    = shadowRay g light ipoint
        Ray (_, dir2light)  = shadowRay'

        reflectedLight = case traceRay scene (Just entity') shadowRay' of
            Environment -> brdfReflEnergy                                           -- shadow ray is traced to the light - 100% diffuse reflection
            Hit t _ _ _ -> if t < distance ipoint (lightPos light) then envEnergy else brdfReflEnergy   -- light is shadowed by some object

        brdfReflEnergy = evalBRDF brdf hit dir2light . eval $ light

imageSample :: RandomGen gen => gen -> Camera cam => Scene -> cam -> UnitSpace -> (Energy, gen)
imageSample g scene camera = method g scene . cameraRay camera

-- Ray-trace whole image viewed by camera
raytrace :: (RandomGen gen, Camera cam) => gen -> Scene -> cam -> (gen, Image PixelRGB8)
raytrace gen scene camera = generateFoldImage pixelColor gen width height where
    pixelColor g x y = (g', mapEnergy e) where
            (e, g') = imageSample g scene camera $ toScreenSpace sensor x y
    sensor@(Sensor (width, height, _)) = cameraSensor camera
