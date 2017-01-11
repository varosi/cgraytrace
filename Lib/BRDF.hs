module BRDF where

import Prelude ()
import Numeric.Units.Dimensional.Prelude
import Geometry
import Light
import Math
import Linear
import System.Random (RandomGen(..))

-- |all BRDFs have this two functions
class BRDF brdf geom where
    evalBRDF     :: brdf -> Intersection geom -> UnitV3 -> UnitV3 -> LightTrans -- intersection info, dir to viewer, dir to light
    generateRay  :: RandomGen gen => gen -> brdf -> Intersection geom -> (Ray, gen) -- generate new ray reflected from the surface


data BRDFs = Diffuse LightTrans
                deriving (Eq, Show)

-- |Implement Diffuse BRDF
instance BRDF BRDFs a where
    evalBRDF (Diffuse reflectivity) hit _ dir2light =
        (cs' *) <$> reflectivity where
            csL = dot (normalized . isectNormal $ hit) (normalized dir2light) *~ one
            cs' = _2 * clamp _0 _1 csL

    generateRay gen (Diffuse _) (Hit _ ipoint inormal itangent ibitangent _) =
        (Ray (ipoint, normalize3 dir), gen'') where

            (ran_theta, gen') = next gen
            (ran_phi, gen'')  = next gen'
            inRange' g x = inRange g x *~ one                       :: PlaneAngle Float

            theta = inRange' gen ran_theta * (pi/_2)                :: PlaneAngle Float
            phi   = inRange' gen' ran_phi * (_2*pi)                 :: PlaneAngle Float
            localDir = fromSpherical( SphereV 1 theta phi )

            surfaceT = V3 (normalized itangent) (normalized ibitangent) (normalized inormal) :: M33 Float
            dir = localDir *! surfaceT
