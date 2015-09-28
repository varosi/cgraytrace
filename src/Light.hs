{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Light where

import Math
import Linear
import Linear.Affine

envEnergy :: Energy
envEnergy = Energy $ V3 (28.0/255) (115.0/255) (136.0/255)

newtype Energy = Energy (V3 Float) deriving Num    -- R, G, B components of energy that we sense

class Shadow light where
    shadowRay :: light -> Coord3 -> Ray
    eval      :: light -> Energy

newtype Brightness = Brightness (V3 Float)

data Light = OmniLight (Coord3, Brightness)

instance Shadow Light where
    shadowRay (OmniLight (pos, _)) point = Ray (point, dir) where
        dir = normalize3( pos .-. point )

    eval (OmniLight (_, Brightness e)) = Energy e