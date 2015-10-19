{-# LANGUAGE FlexibleInstances #-}
module BRDF where

import qualified Prelude as Pr
import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional
import Geometry
import Light
import Math
import Linear
import Linear.Affine
import System.Random (RandomGen(..))

class BRDF brdf geom where
    evalBRDF     :: brdf -> Intersection geom -> Normal -> Normal -> Albedo -- intersection info, dir to viewer, dir to light
    generateRay  :: RandomGen gen => gen -> brdf -> Intersection geom -> (Ray, gen) -- generate new ray reflected/refracted from the surface

data BRDFs = Diffuse Albedo
                deriving (Eq, Show)

transfer :: Float -> Float -> Float -> Albedo
transfer r g b = V3 (Dimensional r) (Dimensional g) (Dimensional b)

instance BRDF BRDFs a where
    evalBRDF (Diffuse reflectivity) (Hit _ _ inormal _) _ dir2light =
        cs' *^ reflectivity where
            csL = dot (normalized inormal) (normalized dir2light)
            cs' = Dimensional $ 2* clamp 0 1 csL

    generateRay gen (Diffuse _) (Hit _ ipoint inormal _) = (Ray (ipoint, normalize3 dir), gen'') where
        (ran_theta, gen') = next gen
        (ran_phi, gen'')  = next gen'

        SphereV _ theta phi = toSpherical . normalized $ inormal
        theta' = inRange gen ran_theta * (pi/2) - (pi/4) + theta
        phi'   = inRange gen' ran_phi * pi - (pi/2) + phi
        dir    = fromSpherical( SphereV 1 theta' phi' )