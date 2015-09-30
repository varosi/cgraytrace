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

data Scene = Scene {    scEntities  :: [Entity],
                        scLights    :: [Light] }

liftIntersection :: Entity -> Intersection Geometry -> Intersection Entity
liftIntersection _ Environment = Environment
liftIntersection (Entity _ mat) (Hit d p n g) = Hit d p n (Entity g mat)

instance Intersectable Entity where
    intersect ray entity@(Entity geom _) = liftIntersection entity . intersect ray $ geom

demoScene :: Scene
demoScene = Scene [sphere0, sphere1, sphere2, plane0] [light0] where
        sphere0 = Entity (Sphere (P$V3 0 0 200) 20) (Mat$Diffuse (V3 0.98 0 0))
        sphere1 = Entity (Sphere (P$V3 5 35 200) 25) (Mat$Diffuse (V3 0 0.98 0))
        sphere2 = Entity (Sphere (P$V3 (-25) 20 180) 10) (Mat$Diffuse (V3 0 0.98 0.98))
        ground  = Entity (Sphere (P$V3 0 (-1030) 200) 1000) (Mat$Diffuse (V3 0.98 0.98 0.98))
        plane0  = Entity (Plane (normalize3(V3 0 0.5 (-0.5))) (150)) (Mat$Diffuse (V3 0.5 0.5 0.5))
        light0  = OmniLight (P$V3 (-40) (80) 0, Brightness 1)

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
