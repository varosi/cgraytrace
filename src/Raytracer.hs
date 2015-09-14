module Raytracer( raytrace, Entity(..), Scene(..), Sensor(..), Camera(..) ) where

import Codec.Picture.Types (Image(..), PixelRGB8(..), generateImage)
import Data.Vector.Storable (generate)
import Linear.V2

-- http://hackage.haskell.org/package/linear

type Entity = Int
type Scene  = [Entity]

newtype ScreenSpace a = SS (V2 a)

newtype Sensor = Sensor (Int, Int) -- width, height
newtype Camera = Camera Sensor

-- Single sample
sample :: Scene -> ScreenSpace Float -> PixelRGB8
sample _ (SS (V2 x y)) = PixelRGB8 a 0 0 where a = if x < 50 then 0 else 0xff

-- Ray-trace whole image viewed by camera
raytrace :: Scene -> Camera -> Image PixelRGB8
raytrace scene (Camera (Sensor (width, height))) = generateImage imageSample width height where
    imageSample x y = sample scene (SS (V2 (fromIntegral x) (fromIntegral y)))