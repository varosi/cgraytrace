{-# LANGUAGE RankNTypes #-}
module Math(    Ray(..), Vec3, Coord3,
                Normal, normalize3, normalized,
                clamp, inRange,
                SphericalVec(..), toSpherical, fromSpherical ) where

import Linear
import Linear.Affine
import System.Random (RandomGen(..))

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

-- Calculate random Word32 numbers into Float [0,1]
inRange :: RandomGen g => g -> Int -> Float
inRange gen i = fromIntegral (i - min') / fromIntegral (max' - min') where
    (min', max') = genRange gen

-- theta The elevation angle in the range [-pi/2, pi/2]
-- phi The azimuth angle in the range [0, 2*pi]
data SphericalVec = SphereV !Float !Float !Float -- length, theta, phi

toSpherical :: V3 Float -> SphericalVec
toSpherical (V3 x y z) = SphereV r theta phi where
        r     = sqrt (x*x + y*y + z*z)
        theta = asin (z/r)
        phi'  = atan2 y x
        phi   = if phi' < 0 then phi' + (2*pi) else phi'

-- theta The elevation angle in the range [-pi/2, pi/2].
-- phi The azimuth angle in the range [0, 2*pi].
fromSpherical :: SphericalVec -> V3 Float
fromSpherical (SphereV r theta phi) = r *^ V3 (cos phi * thetaCos) (sin phi * thetaCos) (sin theta) where
        thetaCos = cos theta