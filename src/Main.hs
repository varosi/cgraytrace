{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

import Yesod
import Codec.Picture.Png
import Raytracer
import Linear
import Linear.Affine
import Math
import Camera
import Scene

-- Little server to show us rendering result
data App = App
instance Yesod App

mkYesod "App" [parseRoutes| / ImageR GET |]

getImageR :: MonadHandler m => m TypedContent
getImageR = sendResponse $ toTypedContent (typePng, toContent (encodePng image)) where
                image    = raytrace demoScene camera

                camera   = OrthoCamera sensor camSize camPos camDir camUp
                sensor   = Sensor (320, 240)
                camPos   = P $ V3 0.0 0.0 0.0
                camDir   = normalize3( V3 0.0 0.0 1.0 )
                camUp    = normalize3( V3 0.0 1.0 0.0 )
                camFocal = 70.0
                camSize  = V2 100.0 100.0

main :: IO ()
main = warpEnv App
