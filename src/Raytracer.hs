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
method :: RandomGen gen => gen -> Scene -> RaySegment -> (Energy, gen)
method = pathTrace

mapEnergy :: Energy -> PixelRGB8
mapEnergy (Energy( P( V3 r g b )) ) = PixelRGB8 (f2w r) (f2w g) (f2w b) where
        f2w f = truncate ((min 1 (f**invGamma)) * 255)

depthMap :: Maybe (Intersection a) -> Energy
depthMap Nothing              = envEnergy
depthMap (Just (Hit t _ _ _)) = Energy . P $ V3 a a a where a = t / 100

traceRay :: Scene -> Maybe Entity -> RaySegment -> Maybe (Intersection Entity)
traceRay scene toSkip raySeg = foldl closest Nothing . map (intersect raySeg) . skip toSkip . scEntities $ scene where
        closest x0 Nothing                                                 = x0
        closest Nothing x@(Just (Hit t _ _ _))                             = if t >= 0 then x else Nothing
        closest x0@(Just (Hit t0 _ _ entity0)) x@(Just (Hit t _ _ entity)) = if (entity /= entity0) && (t >= 0) && (t < t0) then x else x0

        skip Nothing xs  = xs
        skip (Just e) xs = filter (/= e) xs

-- shootMany :: (Num a, Enum a) => (t1 -> t -> (Energy, t1)) -> a -> t1 -> Maybe t -> (Energy, t1)
shootMany _ _ g0 Nothing                = (envEnergy, g0)
shootMany shooter cnt g0 (Just geomHit) = (avgEnergy, g'') where
    (lightSamples, g'') = foldl (\(xs, g) _ -> let (e, g') = shooter g geomHit in (e:xs, g')) ([], g0) [1..cnt]
    avgEnergy           = averageEnergy lightSamples

rayCast :: Scene -> RaySegment -> Energy
rayCast scene = depthMap . traceRay scene Nothing

pathTrace :: RandomGen gen => gen -> Scene -> RaySegment -> (Energy, gen)
pathTrace gen scene = pathTrace' maxDepth Nothing gen where
    maxDepth = rsPathMaxDepth.scSettings $ scene

    pathTrace' 0 _ g _                                       = (envEnergy, g)
    pathTrace' levelsLeft prevHit g0 raySeg@(RaySeg (Ray (_, shootDir), maxt)) = result where
        geomHit        = traceRay scene prevHit raySeg
        dir2viewer     = normalize3( normalized shootDir ^* (-1) )

        isCameraRay    = maxDepth - levelsLeft == 0
        giSamplesCount = if isCameraRay then rsSecondaryGICount.scSettings $ scene else 1

        -- shadow rays shoot first
        (lightEnergy, g'')       = shootMany bounce2light (rsLightSamplesCount.scSettings $ scene) g0 geomHit

        -- gi rays shoot
        result                   = shootMany bounce2GI giSamplesCount g'' geomHit

        bounce2light g hit@(Hit _ ipoint _ entity') = (reflectedLight, g') where
            Mat brdf            = enMaterial entity'
            light               = scLight scene                        -- Single light support currently
            (shadowRaySeg, g')  = shadowRay g light ipoint
            RaySeg (Ray (_, dir2light), _) = shadowRaySeg

            reflectedLight = case traceRay scene (Just entity') shadowRaySeg of
                Nothing -> brdfReflEnergy    -- shadow ray is traced to the light - 100% diffuse reflection
                Just _  -> envEnergy         -- light is shadowed by some object

            brdfReflEnergy = lightEnergy' `attenuateWith` brdfTransfer where
                    lightEnergy' = eval light
                    brdfTransfer = evalBRDF brdf hit dir2viewer dir2light

        -- Next path segment calculations
        bounce2GI gen' hit@(Hit _ _ _ entity) = (totalEnergy, gLast) where
            (irradiance, gLast)          = pathTrace' (levelsLeft-1) (Just entity) g''' (RaySeg (ray', farthestDistance))
            Mat brdf                     = enMaterial entity
            (ray'@(Ray (_, dir')), g''') = generateRay gen' brdf hit -- from BRDF
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