module Geometry where
import Math
import Linear
import Linear.Affine

-- |Intersection information returned from each successful intersection
data Intersection g =   Hit {   isectDepth     :: Float,   -- depth to intersection from ray origin
                                isectPoint     :: Coord3,  -- point of intersection
                                isectNormal    :: UnitV3,  -- normal at the point of intersection
                                isectTangent   :: UnitV3,  -- tangent over the surface at the point of intersection
                                isectBiTangent :: UnitV3,  -- bi-tangent over the surface at the point of intersection
                                isectEntity    :: g }      -- intersected geometry
                                    deriving Show

class Intersectable geom where
    intersect :: RaySegment -> geom -> Maybe (Intersection geom)

-- |All geometric types supported by this raytracer
data Geometry = Sphere Coord3 Float |
                Plane  UnitV3 Float
                    deriving (Eq, Show)

-- |Implementation of Spehere and Plane geometries intersection
instance Intersectable Geometry where

    intersect raySegment sphere@(Sphere center radius) =
        if d >= 0 && t <= maxDepth then Just( Hit t ipoint inormal itangent ibitangent sphere ) else Nothing where
            (RaySeg (Ray (rayOrigin, dir), maxDepth)) = raySegment
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
            itangent    = mkTangent inormal
            ibitangent  = normalize3( cross (normalized inormal) (normalized itangent) )

    intersect raySegment plane@(Plane normal d) =
        if t >= 0 && t <= maxDepth then Just( Hit t point' normal itangent ibitangent plane ) else Nothing where
            (RaySeg (Ray (rayOrigin, dir), maxDepth)) = raySegment
            (P p0)      = rayOrigin
            t           = (-(dot p0 (normalized normal) + d)) / dot (normalized dir) (normalized normal)
            point'      = rayOrigin .+^ (t *^ normalized dir)
            itangent    = mkTangent normal
            ibitangent  = normalize3( cross (normalized normal) (normalized itangent) )
