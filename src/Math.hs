{-# LANGUAGE RankNTypes                #-}
module Math where

import GHC.Word
import Linear
import Linear.Affine

type Vec3   = V3 Float
type Coord3 = Point V3 Float                     -- World coordinate system

newtype Ray = Ray (Coord3, Normal) deriving Show -- position & direction

-- Track normalized type-safe vectors in world-coordinate system
newtype Normal = Normal Vec3 deriving (Eq, Show)

normalize3 :: Vec3 -> Normal
normalize3 = Normal . normalize

normalized :: Normal -> Vec3
normalized (Normal v) = v

clamp :: forall a. Ord a => a -> a -> a -> a
clamp min' max' x = min (max min' x) max'


inRange :: Int -> Float
-- inRange i       = 0.5 * fromIntegral i / fromIntegral (maxBound :: Int)  -- for Mersene
inRange i = 2* fromIntegral i / fromIntegral (maxBound :: Word32)              -- for Std random
