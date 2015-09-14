{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

-- http://hackage.haskell.org/package/linear

import Yesod
import Codec.Picture.Png
import Codec.Picture.Types (Image(..), PixelRGB8)
import Data.Vector.Storable (generate)
import Linear.V3

image :: Image PixelRGB8
image = Image width height myData where
    myData = generate (width*height*3) (\i -> if i `mod` (16*3) == 0 then 0 else 0xFF)
    width  = 320
    height = 240

-- Little server to show us rendering result
data App = App
instance Yesod App

mkYesod "App" [parseRoutes| / ImageR GET |]

getImageR :: MonadHandler m => m TypedContent
getImageR = sendResponse $ toTypedContent (typePng, toContent (encodePng image))

main :: IO ()
main = warpEnv App
