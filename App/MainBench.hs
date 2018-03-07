module Main where

import Criterion.Main
import Raytracer
import Scene
import System.Random.TF.Gen (seedTFGen)

render = image where
    gen = seedTFGen (1,2,3,4)
    image = raytrace gen cornellScene (cornellCamera 1024 768)

main :: IO ()
main = defaultMain [
    bgroup "1024x768" [bench "cornell" $ whnf render]
    ]