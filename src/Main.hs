module Main where

import Codec.Picture.Png
import Codec.Picture.Types (Image(..), PixelRGB8)
import Data.Vector.Storable (generate)

image :: Image PixelRGB8
image = Image width height myData where
    myData = generate (width*height*3) (\i -> if i `mod` (16*3) == 0 then 0 else 0xFF)
    width  = 320
    height = 240

main :: IO ()
main = do
    writePng "test.png" image
