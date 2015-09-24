module Geometry where
import Math
import Linear
import Linear.Affine

data Intersection = Intersection { isIntersected :: Bool }

class Intersectable geom where
    intersect :: Ray -> geom -> Intersection


data Entity = Sphere Coord3 Float

instance Intersectable Entity where
    intersect (Ray (rayOrigin, dir)) (Sphere center radius) = Intersection (d >= 0) where
        ndir        = normalized dir
        (P voffs)   = rayOrigin - center
        ac          = dot ndir ndir                     :: Float
        bc          = 2 * dot voffs ndir                :: Float
        cc          = dot voffs voffs - (radius^2)      :: Float
        d           = bc^2 - 4*ac*cc