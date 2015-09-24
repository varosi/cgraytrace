module Scene where

import Linear
import Linear.Affine
import Geometry

type Scene  = [Entity]

demoScene :: Scene
demoScene = [sphere0, sphere1] where
        sphere0 = Sphere (P $ V3 0 0 100) 20
        sphere1 = Sphere (P $ V3 5 15 100) 20