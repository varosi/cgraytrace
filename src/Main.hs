{-# LANGUAGE NoMonomorphismRestriction, QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
module Main where

import Yesod
import Codec.Picture.Png
import Raytracer
import Scene
import System.Random.TF.Gen (seedTFGen)

-- Little RESTful HTTP web server to show us rendering result
data App = App
instance Yesod App

mkYesod "App" [parseRoutes| / ImageR GET |]

-- |HTTP GET at "host/" address that return us ray traced image
getImageR :: MonadHandler m => m TypedContent
getImageR = do
                let gen   = seedTFGen (1,2,3,4)
                let image = raytrace gen cornellScene cornellCamera

                sendResponse $ toTypedContent (typePng, toContent (encodePng image))

main :: IO ()
main = warpEnv App