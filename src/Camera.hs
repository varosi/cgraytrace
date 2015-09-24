{-# LANGUAGE FlexibleContexts #-}
module Camera where

import Math
import Linear
import Linear.Affine

newtype ScreenSpace = SS (V2 Float)

newtype Sensor = Sensor (Int, Int)                      -- width, height

-- Interface for all cameras
class Camera cam where
    cameraRay    :: cam -> ScreenSpace -> Ray
    cameraSensor :: cam -> Sensor

data PinholeCamera = PinholeCamera {
            phcamSensor      :: Sensor,
            phcamPos         :: Coord3,
            phcamDir         :: Normal,
            phcamUp          :: Normal,
            phcamFocalLength :: Float }

data OrthoCamera = OrthoCamera {
            orthoSensor      :: Sensor,
            orthoSize        :: V2 Float,
            orthoPos         :: Coord3,
            orthoDir         :: Normal,
            orthoUp          :: Normal }

instance Camera OrthoCamera where
      cameraRay cam (SS imagePos) = Ray (start, orthoDir cam) where
          (V2 x y) = (imagePos - (V2 0.5 0.5)) * (orthoSize cam)
          vpos3 = V3 x y 0.0
          start = orthoPos cam .+^ (vpos3 *! view)

          xaxis = normalize( cross (normalized.orthoUp$cam) zaxis )
          yaxis = cross zaxis xaxis
          zaxis = normalized( orthoDir cam )

          view  = V3 xaxis yaxis zaxis                  :: M33 Float

      cameraSensor = orthoSensor

--instance Camera PinholeCamera where
--    cameraRay cam imagePos = Ray (phcamPos cam, normalize3 v) where
--            d        = normalized( phcamDir cam )
--            v        = d ^* (phcamFocalLength cam)
--            (P vpos) = phcamPos cam
--            xaxis    = normalize( cross (phcamUp cam) zaxis )
--            yaxis    = cross zaxis xaxis
--            zaxis    = normalize( phcamPos cam .-^ (normalized.phcamDir $ cam) )
--            view     = m43_to_m44( V4 xaxis yaxis zaxis vpos )
--
--    cameraSensor = phcamSensor

toScreenSpace :: Sensor -> Int -> Int -> ScreenSpace
toScreenSpace (Sensor (width, height)) x y = SS $ V2 sx sy where
        sx = fromIntegral x / (fromIntegral width  - 1)
        sy = fromIntegral y / (fromIntegral height - 1)