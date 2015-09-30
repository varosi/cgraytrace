module Geometry where
import Math
import Linear
import Linear.Affine

data Intersection g =   Environment |
                        Hit {   isectDepth  :: Float,   -- depth to intersection from ray origin
                                isectPoint  :: Coord3,  -- point of intersection
                                isectNormal :: Normal,  -- normal at the point of intersection
                                isectEntity :: g }      -- intersected geometry
                                deriving Show

class Intersectable geom where
    intersect :: Ray -> geom -> Intersection geom

data Geometry = Sphere Coord3 Float |
                Plane  Normal Float
                    deriving (Eq, Show)

instance Intersectable Geometry where
    intersect (Ray (rayOrigin, dir)) sphere@(Sphere center radius) =
        if d >= 0 then Hit t ipoint inormal sphere else Environment where
            ndir        = normalized dir
            (P voffs)   = rayOrigin - center
            ac          = dot ndir ndir                     :: Float
            bc          = 2 * dot voffs ndir                :: Float
            cc          = dot voffs voffs - (radius*radius) :: Float
            d           = (bc*bc) - (4*ac*cc)
            sqD         = sqrt d
            s0          = ((-bc) - sqD) / (2*ac)
            s1          = ((-bc) + sqD) / (2*ac)
            t           = min s0 s1
            ipoint      = rayOrigin .+^ (t *^ normalized dir)
            inormal     = normalize3( ipoint .-. center )

    intersect (Ray (rayOrigin, dir)) plane@(Plane normal d) =
        if t >= 0 then Hit t point' normal plane else Environment where
            (P p0)  = rayOrigin
            t       = (-(dot p0 (normalized normal) + d)) / dot (normalized dir) (normalized normal)
            point'  = rayOrigin .+^ (t *^ normalized dir)