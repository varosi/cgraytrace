module Main where

import           Codec.Picture.Png
import           Data.Time.Clock
import           Raytracer
import           Scene
import           System.Random.TF.Gen (seedTFGen)
import           Data.Compact

main :: IO ()
main = do
    t0 <- getCurrentTime
    sceneCompacted <- cornellScene

    let gen = seedTFGen (1,2,3,4)
    let image = raytrace gen (getCompact sceneCompacted) (cornellCamera 1024 768)

    let filename = "sample.png"

    putStrLn $ "Rendering to " ++ filename ++ "..."
    writePng filename image
    t1 <- getCurrentTime

    putStrLn $ "Time: " ++ show (diffUTCTime t1 t0)
