{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

import Yesod
import Codec.Picture.Png
import Raytracer
import Linear.V3

-- Little server to show us rendering result
data App = App
instance Yesod App

mkYesod "App" [parseRoutes| / ImageR GET |]

getImageR :: MonadHandler m => m TypedContent
getImageR = sendResponse $ toTypedContent (typePng, toContent (encodePng image)) where
                image  = raytrace [] $ Camera sensor camPos
                sensor = Sensor (320, 240)
                camPos = Coord3 $ V3 0.0 0.0 0.0 :: Coord3 Float

main :: IO ()
main = warpEnv App
