{-# LANGUAGE FlexibleInstances #-}
module BRDF where

import Geometry
import Light
import Math
import Linear
import System.Random (RandomGen(..))

class BRDF brdf geom where
    evalBRDF     :: brdf -> Intersection geom -> Normal -> Normal -> EnergyTransfer -- intersection info, dir to viewer, dir to irradiance
    generateRay  :: RandomGen gen => gen -> brdf -> Intersection geom -> (Ray, gen) -- generate new ray reflected/refracted from the surface

data BRDFs = Diffuse EnergyTransfer
                deriving (Eq, Show)

transfer :: Float -> Float -> Float -> EnergyTransfer
transfer r g b = EnergyTrans( V3 r g b )

instance BRDF BRDFs a where
    evalBRDF _ Environment _ _ = undefined
    evalBRDF (Diffuse (EnergyTrans reflectivity)) (Hit _ _ inormal _) dir2view dir2light =
        EnergyTrans( cs' *^ reflectivity ) where
            csV = dot (normalized inormal) (normalized dir2view)
            csL = dot (normalized inormal) (normalized dir2light)
            cs' = clamp 0 1 (csV * csL)

    generateRay _ _ Environment = undefined
    generateRay gen (Diffuse _) (Hit _ ipoint inormal _) = (Ray (ipoint, normalize3 dir), gen'') where
        (ran_theta, gen') = next gen
        (ran_phi, gen'')  = next gen'

        SphereV _ theta phi = toSpherical . normalized $ inormal

        theta' = (((inRange gen ran_theta) * (pi/2)) - (pi/4) + theta)
        phi'   = (((inRange gen' ran_phi) * pi) - (pi/2) + phi)

        dir    = fromSpherical( SphereV 1 theta' phi' )