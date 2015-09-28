module Geometry where
import Material
import Math
import Linear
import Linear.Affine

data Entity = Entity { enGeom :: Geometry, enMaterial :: Material } deriving Eq

data Intersection g =   Environment |
                        Hit {   isectDepth  :: Float,
                                isectPoint  :: Coord3,
                                isectEntity :: g }

liftIntersection :: Entity -> Intersection Geometry -> Intersection Entity
liftIntersection _ Environment = Environment
liftIntersection (Entity _ mat) (Hit d p g) = Hit d p (Entity g mat)

class Intersectable geom where
    intersect :: Ray -> geom -> Intersection geom

data Geometry = Sphere Coord3 Float | Plane Normal Float deriving Eq

instance Bounded Float where
    minBound = -1E+20
    maxBound = 1E+20

instance Intersectable Entity where
    intersect ray entity@(Entity geom mat) = liftIntersection entity . intersect ray $ geom

instance Intersectable Geometry where
    intersect (Ray (rayOrigin, dir)) entity@(Sphere center radius) = 
        if d >= 0 then Hit t point' entity else Environment where
            ndir        = normalized dir
            (P voffs)   = rayOrigin - center
            ac          = dot ndir ndir                     :: Float
            bc          = 2 * dot voffs ndir                :: Float
            cc          = dot voffs voffs - (radius^2)      :: Float
            d           = bc^2 - (4*ac*cc)
            s0          = ((-bc) - d) / (2*ac)
            s1          = ((-bc) + d) / (2*ac)
            t           = min s0 s1
            point'      = rayOrigin .+^ (t *^ normalized dir)

    intersect (Ray (rayOrigin, dir)) entity@(Plane normal d) =
        if t >= 0 then Hit t point' entity else Environment where
            (P p0)  = rayOrigin
            t       = -(dot p0 (normalized dir) + d) / (dot (normalized dir) (normalized normal))
            point'  = rayOrigin .+^ (t *^ normalized dir)