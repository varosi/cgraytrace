module Camera where

import Math
import Linear
import Linear.Affine

newtype UnitSpace = US (V2 Float)

newtype Sensor = Sensor (Int, Int)                      -- width, height

-- Interface for all cameras
class Camera cam where
    cameraRay    :: cam -> UnitSpace -> Ray
    cameraSensor :: cam -> Sensor

data PinholeCamera = PinholeCamera {
            phcamSensor      :: Sensor,
            phcamSize        :: V2 Float,
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
      cameraRay cam (US imagePos) = Ray (start, orthoDir cam) where
          Sensor (w,h)  = orthoSensor cam
          aspect        = fromIntegral w / fromIntegral h :: Float

          (V2 x y)      = (imagePos - V2 0.5 0.5) * orthoSize cam
          vpos3         = V3 (x*aspect) y 0.0
          start         = orthoPos cam .+^ (vpos3 *! view)

          xaxis         = normalize( cross (normalized.orthoUp$cam) zaxis )
          yaxis         = cross zaxis xaxis
          zaxis         = normalized( orthoDir cam )

          view          = V3 xaxis yaxis zaxis                  :: M33 Float

      cameraSensor = orthoSensor

instance Camera PinholeCamera where
    cameraRay cam (US imagePos) = Ray (phcamPos cam, normalize3 proj) where
        Sensor (w,h)  = phcamSensor cam
        aspect        = fromIntegral w / fromIntegral h :: Float

        (V2 x y)      = (imagePos - V2 0.5 0.5) * phcamSize cam
        vpos3         = V3 (x*aspect) (-y) 0.0

        d        = normalized( phcamDir cam )
        v        = d ^* phcamFocalLength cam - vpos3
        proj     = v *! view

        xaxis    = normalize( cross( normalized.phcamUp$cam ) zaxis )
        yaxis    = cross zaxis xaxis
        zaxis    = normalized( phcamDir cam )

        view     = V3 xaxis yaxis zaxis

    cameraSensor = phcamSensor

toScreenSpace :: Sensor -> Int -> Int -> UnitSpace
toScreenSpace (Sensor (width, height)) x y = US $ V2 sx sy where
        sx = fromIntegral x / (fromIntegral width  - 1)
        sy = fromIntegral y / (fromIntegral height - 1)