module Scene where

import Linear
import Linear.Affine
import Geometry
import Camera
import Math

type Scene  = [Entity]

demoScene :: Scene
demoScene = [sphere0, sphere1] where
        sphere0 = Sphere (P $ V3 0 0 100) 20
        sphere1 = Sphere (P $ V3 5 15 100) 20

demoCamera   = OrthoCamera sensor camSize camPos camDir camUp where
        sensor   = Sensor (160, 120)
        camPos   = P $ V3 0 0 0
        camDir   = normalize3( V3 0 0 1 )
        camUp    = normalize3( V3 0 (-1) 0 )
        -- camFocal = 70.0
        camSize  = V2 60 60
