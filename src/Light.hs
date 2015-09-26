module Light where

import Math
import Linear.Affine

envEnergy :: Energy
envEnergy = Energy (28.0/255, 115.0/255, 136.0/255)

newtype Energy = Energy (Float, Float, Float)    -- R, G, B components of energy that we sense

class Shadow light where
    shadowRay :: light -> Coord3 -> Ray
    eval      :: light -> Energy

newtype Brightness = Brightness Float

data Light = OmniLight (Coord3, Brightness)

instance Shadow Light where
    shadowRay (OmniLight (pos, _)) point = Ray (point, dir) where
        dir = normalize3( pos .-. point )

    eval (OmniLight (_, Brightness e)) = Energy (e, e, e)