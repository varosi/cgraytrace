{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Light where

import Math
import Linear
import Linear.Affine

type Color = V3 Float

envEnergy :: Energy
-- envEnergy = Energy $ V3 (28.0/255) (115.0/255) (136.0/255)
envEnergy = Energy $ V3 (0/255) (0/255) (0/255)

newtype Energy = Energy Color deriving Num    -- R, G, B components of energy that we sense

class Shadow light where
    shadowRay :: light -> Coord3 -> Ray
    eval      :: light -> Energy

newtype Brightness = Brightness Color

data Light = OmniLight (Coord3, Brightness)

instance Shadow Light where
    shadowRay (OmniLight (pos, _)) point' = Ray (point', dir) where
        dir = normalize3( pos .-. point' )

    eval (OmniLight (_, Brightness e)) = Energy e