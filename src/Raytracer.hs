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

-- |Trace single ray segment against a scene and return closest intersection if there is
traceRay :: Scene -> Maybe Entity -> RaySegment -> Maybe (Intersection Entity)
traceRay scene toSkip raySeg = foldl closest Nothing . map (intersect raySeg) . skip toSkip . scEntities $ scene where
        closest x0 Nothing                                                 = x0
        closest Nothing x@(Just (Hit t _ _ _ _ _))                         = if t >= 0 then x else Nothing
        closest x0@(Just (Hit t0 _ _ _ _ entity0)) x@(Just (Hit t _ _ _ _ entity)) = if (entity /= entity0) && (t >= 0) && (t < t0) then x else x0

        skip Nothing xs  = xs
        skip (Just e) xs = filter (/= e) xs

-- |Shoot many rays using "shooter" function
shootMany :: (Num a, Enum a) => (t1 -> t -> (LightIntensity, t1)) -> a -> t1 -> LightIntensity -> Maybe t -> (LightIntensity, t1)
shootMany _ _ g0 envLightIntensity Nothing  = (envLightIntensity, g0)
shootMany shooter cnt g0 _ (Just geomHit)   = (avgLightIntensity, g'') where
    (samples, g'') = foldl (\(xs, g) _ -> let (e, g') = shooter g geomHit in (e:xs, g')) ([], g0) [1..cnt]
    avgLightIntensity = averageIntensity samples

-- |Trace single path of rays
pathTrace :: RandomGen gen => gen -> Scene -> RaySegment -> (LightIntensity, gen)
pathTrace gen scene = pathTrace' maxDepth Nothing gen where
    maxDepth = rsPathMaxDepth.scSettings $ scene

    -- |Helper separation so we could reuse scene and initial conditions across recursive calls
    pathTrace' 0 _ g _                      = (scEnvLight scene, g)
    pathTrace' levelsLeft prevHit g0 raySeg = ((+) <$> lightIntensity <*> giIntensity, g''') where

        (RaySeg (Ray (_, shootDir), _)) = raySeg
        geomHit        = traceRay scene prevHit raySeg
        dir2viewer     = normalize3( normalized shootDir ^* (-1) )

        isCameraRay    = maxDepth - levelsLeft == 0
        giSamplesCount = if isCameraRay then rsSecondaryGICount.scSettings $ scene else 1

        -- Trace shadow rays and GI rays
        (lightIntensity, g'') = shootMany bounce2light (rsLightSamplesCount.scSettings $ scene) g0  (scEnvLight scene) geomHit     -- shadow rays shoot first
        (giIntensity, g''')   = shootMany bounce2GI    giSamplesCount                           g'' (scEnvLight scene) geomHit     -- gi rays shoot

        -- |Shadow rays tracing
        bounce2light g hit@(Hit _ ipoint _ _ _ entity) = (reflectedLight, g') where
            light               = scLight scene
            (shadowRaySeg, g')  = shadowRay g light ipoint
            RaySeg (Ray (_, dir2light), _) = shadowRaySeg

            reflectedLight = case traceRay scene (Just entity) shadowRaySeg of
                Nothing -> radiance             -- shadow ray is traced to the light
                Just _  -> zeroLightIntensity   -- light is shadowed by some object

            irradiance  = eval light dir2light
            radiance    = evalHitBRDF hit irradiance dir2light

        -- |GI tracing
        bounce2GI g hit@(Hit { isectEntity = entity }) = (exRadiance, gLast) where
            (inRadiance, gLast)             = pathTrace' (levelsLeft-1) (Just entity) g' (RaySeg (nextRay, farthestDistance))   -- recursive path tracing
            Mat brdf                        = enMaterial entity
            (nextRay@(Ray (_, inDir)), g')  = generateRay g brdf hit                -- from BRDF
            exRadiance                      = evalHitBRDF hit inRadiance inDir

        -- |Evaluate BRDF of hit surfaces and return radiance
        evalHitBRDF hit irradiance irrDir = radiance where
            Mat brdf        = enMaterial . isectEntity $ hit
            brdfTransfer    = evalBRDF brdf hit dir2viewer irrDir
            radiance        = irradiance `attenuateWith` brdfTransfer

-- |Trace single path of rays from camera space parameters (image sampler)
imageSample :: RandomGen gen => gen -> Camera cam => Scene -> cam -> UnitSpace -> (LightIntensity, gen)
imageSample g scene camera = pathTrace g scene . cameraRay camera

-- |Ray-trace whole image viewed by camera in parallel
raytrace :: (RandomGen gen, Camera cam) => gen -> Scene -> cam -> Image PixelRGB8
raytrace gen scene camera = Image width height raw where
    raw    = fromList( concatMap fromPixel pxList )

    -- |Introduce parallelism at this line
    pxList = pixels gen `using` parBuffer 16 rseq

    -- |Map light intensity to pixel color
    mapLightIntensity :: LightIntensity -> PixelRGB8
    mapLightIntensity (V3 r g b) = PixelRGB8 (f2w r) (f2w g) (f2w b) where
            f2w f = truncate (min 1 (((f /~ lumen) * exposure) ** invGamma) * 255)

    -- |Raytraced pixels
    pixels gen' = map pixel $ zip (generators gen') (screenCoords sensor) where
            pixel (g, (x,y)) = mapLightIntensity . fst $ imageSample g scene camera $ toScreenSpace sensor x y

    fromPixel (PixelRGB8 !r !g !b)               = [r, g, b]
    sensor@(Sensor (width, height, _, exposure)) = cameraSensor camera

-- |Screen coordinates [(0,0), (1,0), (2,0), ...]
screenCoords :: Sensor -> [(Int, Int)]
screenCoords (Sensor (width, height, _, _)) = [(x,y) | y<-[0..height-1], x<-[0..width-1]]

-- |Infinite list of random generators starting from one
generators :: RandomGen gen => gen -> [gen]
generators g = g':generators g' where (_, g')  = split g