module Scene where

import Linear
import Linear.Affine
import Geometry

type Scene  = [Entity]

demoScene :: Scene
demoScene = [sphere0] where
        sphere0 = Sphere (P $ V3 0 0 100) 20