module Main where

import Criterion.Main
import Raytracer
import Scene
import System.Random.TF.Gen (seedTFGen)
import Data.Compact

render (w,h,scene) = image where
    gen = seedTFGen (1,2,3,4)
    image = raytrace gen scene (cornellCamera w h)

main :: IO ()
main = do
    sceneCompacted <- cornellScene

    defaultMain [
        bgroup "cornell" [bench "1024x768" $ nf render (1024,768,(getCompact sceneCompacted))]
        ]