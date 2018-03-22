{-# LANGUAGE NoMonomorphismRestriction, QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings, ViewPatterns #-}
module Main where

import Yesod
import Codec.Picture.Png
import Raytracer
import Scene
import System.Random.TF.Gen (seedTFGen)
import Data.Compact

-- Little RESTful HTTP web server to show us rendering result
data App = App
instance Yesod App

mkYesod "App" [parseRoutes| 
/               ImageR      GET
/res/#Int/#Int  ImageSizeR  GET
|]

getImageSizeR :: MonadHandler m => Int -> Int -> m TypedContent
getImageSizeR width height = do
    sceneCompacted <- liftIO $ cornellScene

    let gen = seedTFGen (1,2,3,4)
    let image = raytrace gen (getCompact sceneCompacted) (cornellCamera width height)

    sendResponse $ toTypedContent (typePng, toContent (encodePng image))

-- |HTTP GET at "host/" address that return us ray traced image
getImageR :: MonadHandler m => m TypedContent
getImageR = getImageSizeR 320 240

main :: IO ()
main = warpEnv App