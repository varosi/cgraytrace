{-# LANGUAGE BangPatterns #-}
module Raytracer( raytrace, Geometry, Scene, Sensor(..), Camera(..), traceRay, pathTrace ) where

import Prelude ((-), (/), (**), (*))
import Numeric.Units.Dimensional.Prelude hiding ((-), (/), (**), (*))
import Camera
import Geometry
import Codec.Picture.Types (Image(..), PixelRGB8(..))
import Data.Vector.Storable (fromList)
import Scene
import Math
import Light
import Linear
import Material
import BRDF
import System.Random (RandomGen(..))
import Control.Parallel.Strategies
import Control.Applicative

gamma, invGamma :: Float
gamma       = 2.2
invGamma    = 1 / gamma

traceRay :: Scene -> Maybe Entity -> RaySegment -> Maybe (Intersection Entity)
traceRay scene toSkip raySeg = foldl closest Nothing . map (intersect raySeg) . skip toSkip . scEntities $ scene where
        closest x0 Nothing                                                 = x0
        closest Nothing x@(Just (Hit t _ _ _))                             = if t >= 0 then x else Nothing
        closest x0@(Just (Hit t0 _ _ entity0)) x@(Just (Hit t _ _ entity)) = if (entity /= entity0) && (t >= 0) && (t < t0) then x else x0

        skip Nothing xs  = xs
        skip (Just e) xs = filter (/= e) xs

shootMany :: (Num a, Enum a) => (t1 -> t -> (LightIntensity, t1)) -> a -> t1 -> Maybe t -> (LightIntensity, t1)
shootMany _ _ g0 Nothing                = (envLightIntensity, g0)
shootMany shooter cnt g0 (Just geomHit) = (avgLightIntensity, g'') where
    (lightSamples, g'') = foldl (\(xs, g) _ -> let (e, g') = shooter g geomHit in (e:xs, g')) ([], g0) [1..cnt]
    avgLightIntensity   = averageIntensity lightSamples

pathTrace :: RandomGen gen => gen -> Scene -> RaySegment -> (LightIntensity, gen)
pathTrace gen scene = pathTrace' maxDepth Nothing gen where
    maxDepth = rsPathMaxDepth.scSettings $ scene

    pathTrace' 0 _ g _                                                      = (envLightIntensity, g)
    pathTrace' levelsLeft prevHit g0 raySeg@(RaySeg (Ray (_, shootDir), _)) = result where
        geomHit        = traceRay scene prevHit raySeg
        dir2viewer     = normalize3( normalized shootDir ^* (-1) )

        isCameraRay    = maxDepth - levelsLeft == 0
        giSamplesCount = if isCameraRay then rsSecondaryGICount.scSettings $ scene else 1

        (lightLightIntensity, g'') = shootMany bounce2light (rsLightSamplesCount.scSettings $ scene) g0  geomHit     -- shadow rays shoot first
        result                     = shootMany bounce2GI    giSamplesCount                           g'' geomHit     -- gi rays shoot

        bounce2light g hit@(Hit _ ipoint _ entity') = (reflectedLight, g') where
            Mat brdf            = enMaterial entity'
            light               = scLight scene
            (shadowRaySeg, g')  = shadowRay g light ipoint

            reflectedLight = case traceRay scene (Just entity') shadowRaySeg of
                Nothing -> brdfReflLightIntensity    -- shadow ray is traced to the light - 100% diffuse reflection
                Just _  -> zeroLightIntensity        -- light is shadowed by some object

            brdfReflLightIntensity = lightLightIntensity' `attenuateWith` brdfTransfer where
                    RaySeg (Ray (_, dir2light), _) = shadowRaySeg
                    lightLightIntensity'           = eval light
                    brdfTransfer                   = evalBRDF brdf hit dir2viewer dir2light

        -- Next path segment calculations
        bounce2GI gen' hit@(Hit _ _ _ entity) = (totalLightIntensity, gLast) where
            (irradiance, gLast)          = pathTrace' (levelsLeft-1) (Just entity) g''' (RaySeg (ray', farthestDistance))
            Mat brdf                     = enMaterial entity
            (ray'@(Ray (_, dir')), g''') = generateRay gen' brdf hit -- from BRDF
            brdfTransfer                 = evalBRDF brdf hit dir2viewer dir'
            totalLightIntensity          = (+) <$> lightLightIntensity <*> (irradiance `attenuateWith` brdfTransfer)

imageSample :: RandomGen gen => gen -> Camera cam => Scene -> cam -> UnitSpace -> (LightIntensity, gen)
imageSample g scene camera = pathTrace g scene . cameraRay camera

-- Ray-trace whole image viewed by camera
raytrace :: (RandomGen gen, Camera cam) => gen -> Scene -> cam -> Image PixelRGB8
raytrace gen scene camera = Image width height raw where
    raw    = fromList( concatMap fromPixel pxList )
    pxList = pixels gen `using` parBuffer 16 rseq   -- whole parallelism

    mapLightIntensity :: LightIntensity -> PixelRGB8
    mapLightIntensity (V3 r g b) = PixelRGB8 (f2w r) (f2w g) (f2w b) where
            f2w f = truncate (min 1 (((f /~ lumen) * exposure) ** invGamma) * 255)

    pixels gen' = map pixel $ zip (generators gen') (screenCoords sensor) where
            pixel (g, (x,y)) = mapLightIntensity . fst $ imageSample g scene camera $ toScreenSpace sensor x y

    fromPixel (PixelRGB8 !r !g !b)               = [r, g, b]
    sensor@(Sensor (width, height, _, exposure)) = cameraSensor camera

screenCoords :: Sensor -> [(Int, Int)]
screenCoords (Sensor (width, height, _, _)) = [(x,y) | y<-[0..height-1], x<-[0..width-1]]

generators :: RandomGen gen => gen -> [gen]
generators g = g':generators g' where (_, g')  = split g