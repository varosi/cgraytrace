module Geometry where
import Math
import Linear
import Linear.Affine

data Intersection = Intersection {
    isIntersected :: Bool,
    depth         :: Float }

class Intersectable geom where
    intersect :: Ray -> geom -> Intersection

data Entity = Sphere Coord3 Float

instance Bounded Float where
    minBound = -1E+20
    maxBound = 1E+20

instance Intersectable Entity where
    intersect (Ray (rayOrigin, dir)) (Sphere center radius) = Intersection isected zdepth where
        ndir        = normalized dir
        (P voffs)   = rayOrigin - center
        ac          = dot ndir ndir                     :: Float
        bc          = 2 * dot voffs ndir                :: Float
        cc          = dot voffs voffs - (radius^2)      :: Float
        d           = bc^2 - 4*ac*cc
        s0          = (-bc - d) / (2*ac)
        s1          = (-bc + d) / (2*ac)
        t           = min s0 s1
        isected     = d >= 0
        zdepth      = if isected then t else maxBound