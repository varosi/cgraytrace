module Main where

import Criterion.Main
import Raytracer
import Scene
import System.Random.TF.Gen (seedTFGen)

render (w,h) = image where
    gen = seedTFGen (1,2,3,4)
    image = raytrace gen cornellScene (cornellCamera w h)

main :: IO ()
main = defaultMain [
    bgroup "cornell" [bench "1024x768" $ nf render (1024,768)]
    ]