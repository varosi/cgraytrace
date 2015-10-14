module Scene where

import Linear
import Linear.Affine
import Geometry
import Camera
import Light
import Material
import BRDF
import Math

data Entity = Entity {  enGeom      :: Geometry,
                        enMaterial  :: Material }
                        deriving (Eq, Show)

data RenderSettings = Settings {
                        rsLightSamplesCount :: Int,
                        rsSecondaryGICount  :: Int,
                        rsPathMaxDepth      :: Int }

data Scene = Scene {    scEntities  :: [Entity],
                        scLight     :: Light,
                        scSettings  :: RenderSettings }

liftIntersection :: Entity -> Intersection Geometry -> Intersection Entity
liftIntersection (Entity _ mat) (Hit d p n g) = Hit d p n (Entity g mat)

instance Intersectable Entity where
    intersect ray entity@(Entity geom _) = fmap (liftIntersection entity) . intersect ray $ geom

mkDiffuse :: Float -> Float -> Float -> Material
mkDiffuse r g b = Mat$Diffuse (transfer r g b)

cornellScene :: Scene
cornellScene = Scene [leftWall, rightWall, bottomWall, backWall, topWall, sphere0] light1 settings where
        sphere0    = Entity (Sphere (P$V3 0 (-50) 0) 20)            (mkDiffuse 0.80 0.80 0)
        leftWall   = Entity (Plane (normalize3(V3 1 0 0)) (100))    (mkDiffuse 0.28 0 0)
        rightWall  = Entity (Plane (normalize3(V3 (-1) 0 0)) (100)) (mkDiffuse 0.0 0.0 0.28)
        bottomWall = Entity (Plane (normalize3(V3 0 1 0)) (100))    (mkDiffuse 0.28 0.28 0.28)
        backWall   = Entity (Plane (normalize3(V3 0 0 (-1))) (100)) (mkDiffuse 0.18 0.18 0.18)
        topWall    = Entity (Plane (normalize3(V3 0 (-1) 0)) (100)) (mkDiffuse 0.18 0.18 0.18)

        light0     = OmniLight (P$V3 0 80 0, Brightness 5)
        light1     = RectLight (P$V3 0 85 0, V3 40 0 0, V3 0 0 40, Brightness 3)

        settings = Settings { rsLightSamplesCount = 10, rsSecondaryGICount = 5, rsPathMaxDepth = 4 }

cornellCamera = PinholeCamera sensor camPos' camDir' camUp' camFocal  where
        sensor   = Sensor (360, 240, camSize)
        -- sensor   = Sensor (1280, 1024, camSize)
        camPos' = P $ V3 0 0 (-80)
        camDir' = normalize3( V3 0 0 1 )
        camUp'  = normalize3( V3 0 (-1) 0 )
        camFocal = 1.0        -- 10mm
        camSize  = V2 3.6 2.4 -- 35mm

demoScene :: Scene
demoScene = Scene [sphere0, sphere1, sphere2, plane0] light0 settings where
        sphere0 = Entity (Sphere (P$V3 0 0 200) 20)                  (mkDiffuse 0.98 0 0)
        sphere1 = Entity (Sphere (P$V3 5 35 200) 25)                 (mkDiffuse 0 0.98 0)
        sphere2 = Entity (Sphere (P$V3 (-25) 20 180) 10)             (mkDiffuse 0 0.98 0.98)
        plane0  = Entity (Plane (normalize3(V3 0 0.5 (-0.5))) (150)) (mkDiffuse 0.5 0.5 0.5)
        light0  = OmniLight (P$V3 (-40) 80 0, Brightness 1)
        settings = Settings { rsLightSamplesCount = 1, rsSecondaryGICount = 20, rsPathMaxDepth = 3 }

demoCamera = demoCamera1

camPos :: Coord3
camPos = P $ V3 0 0 0

camDir, camUp :: Normal
camDir = normalize3( V3 0 0 1 )
camUp  = normalize3( V3 0 (-1) 0 )

demoCamera0 :: OrthoCamera
demoCamera0   = OrthoCamera sensor camPos camDir camUp where
        sensor   = Sensor (160, 160, camSize)
        camSize  = V2 100 100

demoCamera1 :: PinholeCamera
demoCamera1 = PinholeCamera sensor camPos camDir camUp camFocal  where
        sensor   = Sensor (360, 240, camSize)
        camFocal = 4.0        -- 40mm
        camSize  = V2 3.6 2.4 -- 35mm
