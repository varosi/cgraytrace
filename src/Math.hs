{-# LANGUAGE RankNTypes #-}
module Math(    Ray(..), Vec3, Coord3,
                Normal, normalize3, normalized,
                clamp, inRange,
                SphericalVec(..), toSpherical, fromSpherical ) where

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

-- Used to calculate random Word32 numbers into Float
inRange :: Int -> Float
-- inRange i       = 0.5 * fromIntegral i / fromIntegral (maxBound :: Int)  -- for Mersene
inRange i = 2* fromIntegral i / fromIntegral (maxBound :: Word32)           -- for Std random


data SphericalVec = SphereV !Float !Float !Float

toSpherical :: V3 Float -> SphericalVec
toSpherical (V3 x y z) = SphereV r theta phi where
        r     = sqrt (x*x + y*y + z*z)
        theta = acos (z/r)
        phi   = atan (y/x)

fromSpherical :: SphericalVec -> V3 Float
fromSpherical (SphereV r theta phi) = V3 (sin theta * cos phi) (sin theta * sin phi) (cos theta)
