{-# LANGUAGE FlexibleContexts #-}
module Camera where

import Math
import Linear.V2
import Linear.V3
import Linear.Epsilon
import Linear.Vector

newtype ScreenSpace = SS (V2 Float)

newtype Sensor = Sensor (Int, Int)                      -- width, height

data Camera    = Camera { camSensor      :: Sensor,
                          camPos         :: Coord3,
                          camDir         :: Normal,
                          camFocalLength :: Float }

cameraRay :: Camera -> ScreenSpace -> Ray
cameraRay cam imagePos = Ray (camPos cam, normalize3 v) where
        d   = normalized $ camDir cam
        v   = d ^* (camFocalLength cam)

-- Image space to screen space
toScreenSpace :: (Integral i, Fractional f) => Sensor -> i -> i -> ScreenSpace
toScreenSpace (Sensor (width, height)) x y = SS $ V2 sx sy where
        sx = fromIntegral x / (fromIntegral width  - 1)
        sy = fromIntegral y / (fromIntegral height - 1)