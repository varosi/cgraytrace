module Math( Coord3, Normal, normalized, normalize3, Ray(..) ) where

import Linear
import Linear.Affine

type Vec3   = V3 Float
type Coord3 = Point V3 Float              -- World coordinate system

newtype Ray = Ray (Coord3, Normal)        -- position & direction

-- Track normalized type-safe vectors in world-coordinate system
newtype Normal = Normal Vec3

normalize3 :: Vec3 -> Normal
normalize3 = Normal . normalize

normalized :: Normal -> Vec3
normalized (Normal v) = v