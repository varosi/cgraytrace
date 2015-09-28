module Camera where

import Math
import Linear
import Linear.Affine

newtype UnitSpace = US (V2 Float)

newtype Sensor = Sensor (Int, Int, V2 Float)    -- width, height, physical size

sensorAspect :: Sensor -> Float
sensorAspect (Sensor (w,h,V2 sw sy)) =
    fromIntegral w / fromIntegral h / sizeAspect where
        sizeAspect = sw/sy

-- Interface for all cameras
class Camera cam where
    cameraRay    :: cam -> UnitSpace -> Ray
    cameraSensor :: cam -> Sensor

data PinholeCamera = PinholeCamera {
            phcamSensor      :: Sensor,
            phcamPos         :: Coord3,
            phcamDir         :: Normal,
            phcamUp          :: Normal,
            phcamFocalLength :: Float }

data OrthoCamera = OrthoCamera {
            orthoSensor      :: Sensor,
            orthoPos         :: Coord3,
            orthoDir         :: Normal,
            orthoUp          :: Normal }

instance Camera OrthoCamera where
      cameraRay cam (US imagePos) = Ray (start, orthoDir cam) where
          Sensor (_,_,sensorSize)  = orthoSensor cam
          aspect        = sensorAspect.orthoSensor $ cam

          (V2 x y)      = (imagePos - V2 0.5 0.5) * sensorSize
          vpos3         = V3 (x*aspect) y 0.0
          start         = orthoPos cam .+^ (vpos3 *! view)

          xaxis         = normalize( cross (normalized.orthoUp$cam) zaxis )
          yaxis         = cross zaxis xaxis
          zaxis         = normalized( orthoDir cam )

          view          = V3 xaxis yaxis zaxis                  :: M33 Float

      cameraSensor = orthoSensor

instance Camera PinholeCamera where
    cameraRay cam (US imagePos) = Ray (phcamPos cam, normalize3 proj) where
        Sensor (_,_,sensorSize) = phcamSensor cam
        aspect   = sensorAspect.phcamSensor $ cam

        (V2 x y) = (imagePos - V2 0.5 0.5) * sensorSize
        vpos3    = V3 (x*aspect) (-y) 0.0

        d        = normalized( phcamDir cam )
        v        = d ^* phcamFocalLength cam - vpos3
        proj     = v *! view

        xaxis    = normalize( cross( normalized.phcamUp$cam ) zaxis )
        yaxis    = cross zaxis xaxis
        zaxis    = normalized( phcamDir cam )

        view     = V3 xaxis yaxis zaxis

    cameraSensor = phcamSensor

toScreenSpace :: Sensor -> Int -> Int -> UnitSpace
toScreenSpace (Sensor (width, height, _)) x y = US $ V2 sx sy where
        sx = fromIntegral x / (fromIntegral width  - 1)
        sy = fromIntegral y / (fromIntegral height - 1)