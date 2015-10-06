{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

import Yesod
import Codec.Picture.Png
import Raytracer
import Scene
import System.Random.TF.Gen (seedTFGen)
import System.Random (RandomGen(..))
import System.Random.Mersenne.Pure64( newPureMT )

-- Little server to show us rendering result
data App = App
instance Yesod App

mkYesod "App" [parseRoutes| / ImageR GET |]

getImageR :: MonadHandler m => m TypedContent
getImageR = do
                --gen <- liftIO newPureMT
                -- gen <- liftIO newStdGen
                let gen = seedTFGen (1,2,3,4)
                let (_, image) = raytrace gen cornellScene cornellCamera
                sendResponse $ toTypedContent (typePng, toContent (encodePng image))

main :: IO ()
main = warpEnv App
