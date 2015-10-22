{-# LANGUAGE RankNTypes #-}
module Math(    Ray(..), RaySegment (..),
                Vec3, Coord3, coord,
                UnitV3, normalize3, normalized,
                clamp, inRange,
                farthestDistance ) where

import Linear
import Linear.Affine
import System.Random (RandomGen(..))

farthestDistance :: Float
farthestDistance = 1e+10

type Vec3   = V3 Float
type Coord3 = Point V3 Float                     -- World coordinate system

coord :: a -> a -> a -> Point V3 a
coord x y z = P(V3 x y z)

newtype Ray = Ray (Coord3, UnitV3)       deriving Show -- position & direction
newtype RaySegment = RaySeg (Ray, Float) deriving Show -- ray segment over Ray and between [0..Float]

-- Track normalized type-safe vectors in world-coordinate system
newtype UnitV3 = UnitV3 Vec3 deriving (Eq, Show)

normalize3 :: Vec3 -> UnitV3
normalize3 = UnitV3 . normalize

normalized :: UnitV3 -> Vec3
normalized (UnitV3 v) = v

clamp :: forall a. Ord a => a -> a -> a -> a
clamp min' max' x = min (max min' x) max'

-- Calculate random Word32 numbers into Float [0,1]
inRange :: RandomGen g => g -> Int -> Float
inRange gen i = fromIntegral (i - min') / fromIntegral (max' - min') where
    (min', max') = genRange gen
