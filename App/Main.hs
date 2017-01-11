module Main where

import Codec.Picture.Png
import Raytracer
import Scene
import System.Random.TF.Gen (seedTFGen)

main :: IO ()
main = do
    let gen = seedTFGen (1,2,3,4)
    let image = raytrace gen cornellScene (cornellCamera 1024 768)
    let filename = "sample.png"

    putStrLn $ "Rendering to " ++ filename ++ "..."
    writePng filename image
