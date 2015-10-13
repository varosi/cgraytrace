module Raytracer( raytrace, Geometry, Scene, Sensor(..), Camera(..), traceRay, pathTrace ) where

import Camera
import Geometry
import Codec.Picture.Types (Image(..), PixelRGB8(..), generateFoldImage)
import Scene
import Math
import Light
import Linear
import Linear.Affine
import Material
import BRDF
import System.Random (RandomGen(..))
import Control.Parallel
-- import Control.DeepSeq

gamma       = 2.2
invGamma    = 1/gamma

-- rayCast (with depth) or pathTrace
method :: RandomGen gen => gen -> Scene -> Ray -> (Energy, gen)
method = pathTrace

mapEnergy :: Energy -> PixelRGB8
mapEnergy (Energy( P( V3 r g b )) ) = PixelRGB8 (f2w r) (f2w g) (f2w b) where
        f2w f = truncate ((min 1 (f**invGamma)) * 255)

depthMap :: Intersection a -> Energy
depthMap Environment = envEnergy
depthMap (Hit t _ _ _) = Energy . P $ V3 a a a where a = t / 100

traceRay :: Scene -> Maybe Entity -> Ray -> Intersection Entity
traceRay scene toSkip ray = foldl closest Environment . map (intersect ray) . skip toSkip . scEntities $ scene where
        closest x0 Environment                                = x0
        closest Environment x@(Hit t _ _ _)                   = if t >= 0 then x else Environment
        closest x0@(Hit t0 _ _ entity0) x@(Hit t _ _ entity)  = if (entity /= entity0) && (t >= 0) && (t < t0) then x else x0

        skip Nothing xs  = xs
        skip (Just e) xs = filter (/= e) xs

shootMany :: (Num a, Enum a) => (t1 -> t -> (Energy, t1)) -> a -> t1 -> t -> (Energy, t1)
shootMany shooter cnt g0 geomHit = (avgEnergy, g'') where
    (lightSamples, g'') = foldl (\(xs, g) _ -> let (e, g') = shooter g geomHit in (e:xs, g')) ([], g0) [1..cnt]
    avgEnergy           = averageEnergy lightSamples

rayCast :: Scene -> Ray -> Energy
rayCast scene = depthMap . traceRay scene Nothing

pathTrace :: RandomGen gen => gen -> Scene -> Ray -> (Energy, gen)
pathTrace gen scene = pathTrace' maxDepth Nothing gen scene where
    maxDepth = rsPathMaxDepth.scSettings $ scene

    pathTrace' 0 _ g _ _                        = (envEnergy, g)
    pathTrace' levelsLeft prevHit g0 scene ray@(Ray (_, shootDir)) = result where
        geomHit        = traceRay scene prevHit ray
        dir2viewer     = normalize3( normalized shootDir ^* (-1) )

        isCameraRay    = maxDepth - levelsLeft == 0
        giSamplesCount = if isCameraRay then rsSecondaryGICount.scSettings $ scene else 1

        -- shadow rays shoot first
        (lightEnergy, g'')       = shootMany bounce2light (rsLightSamplesCount.scSettings $ scene) g0 geomHit

        -- gi rays shoot
        result@(giEnergy, g'''') = shootMany bounce2GI giSamplesCount g'' geomHit

        bounce2light g Environment                  = (envEnergy, g)
        bounce2light g hit@(Hit _ ipoint _ entity') = (reflectedLight, g') where
            Mat brdf            = enMaterial entity'
            light               = head . scLights $ scene                        -- Single light support currently
            (shadowRay', g')    = shadowRay g light ipoint
            Ray (_, dir2light)  = shadowRay'

            reflectedLight = case traceRay scene (Just entity') shadowRay' of
                Environment -> brdfReflEnergy                                           -- shadow ray is traced to the light - 100% diffuse reflection
                Hit t _ _ _ -> if t < distance ipoint (lightPos light) then envEnergy else brdfReflEnergy   -- light is shadowed by some object

            brdfReflEnergy = lightEnergy' `attenuateWith` brdfTransfer where
                    lightEnergy' = eval light
                    brdfTransfer = evalBRDF brdf hit dir2viewer dir2light

        -- Next path segment calculations
        bounce2GI g0 Environment            = (envEnergy, g0)              -- environment irradiance
        bounce2GI g0 hit@(Hit _ _ _ entity) = (totalEnergy, gLast) where
                (irradiance, gLast) = pathTrace' (levelsLeft-1) (Just entity) g''' scene ray'
                Mat brdf                     = enMaterial entity
                (ray'@(Ray (_, dir')), g''') = generateRay g0 brdf hit -- from BRDF
                brdfTransfer                 = evalBRDF brdf hit dir2viewer dir'
                totalEnergy                  = lightEnergy + (irradiance `attenuateWith` brdfTransfer)

imageSample :: RandomGen gen => gen -> Camera cam => Scene -> cam -> UnitSpace -> (Energy, gen)
imageSample g scene camera = method g scene . cameraRay camera

-- Ray-trace whole image viewed by camera
raytrace :: (RandomGen gen, Camera cam) => gen -> Scene -> cam -> (gen, Image PixelRGB8)
raytrace gen scene camera = generateFoldImage pixelColor gen width height where
    pixelColor g x y = (g', e `par` mapEnergy e) where
            (e, _)   = imageSample g scene camera $ toScreenSpace sensor x y
            (_, g')  = split g          -- make new generators
    sensor@(Sensor (width, height, _)) = cameraSensor camera
