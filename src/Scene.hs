module Scene where

import Linear
import Linear.Affine
import Geometry
import Camera
import Light
import Math

data Scene = Scene {    scEntities  :: [Entity],
                        scLights    :: [Light] }

demoScene :: Scene
demoScene = Scene [sphere0, sphere1] [light0] where
        sphere0 = Sphere (P$V3 0 0 100) 20
        sphere1 = Sphere (P$V3 5 15 100) 17
        light0  = SpotLight (P$V3 0 (-50) 100, Brightness 1)

demoCamera :: OrthoCamera
demoCamera   = OrthoCamera sensor camSize camPos camDir camUp where
        sensor   = Sensor (160, 120)
        camPos   = P $ V3 0 0 0
        camDir   = normalize3( V3 0 0 1 )
        camUp    = normalize3( V3 0 (-1) 0 )
        -- camFocal = 70.0
        camSize  = V2 60 60
