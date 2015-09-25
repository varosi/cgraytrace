module Light where

import Math
import Linear
import Linear.Affine

class Shadow light where
    shadowRay :: light -> Coord3 -> Ray

newtype Brightness = Brightness Float

data Light = SpotLight (Coord3, Brightness)

instance Shadow Light where
    shadowRay (SpotLight (pos, _)) point = Ray (point, dir) where
        dir = normalize3( pos .-. point )