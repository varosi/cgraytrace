{-# LANGUAGE FlexibleInstances #-}
module BRDF where

import Geometry
import Light
import Math
import Linear
import System.Random (RandomGen(..))

class BRDF brdf geom where
    evalBRDF     :: brdf -> Intersection geom -> Normal -> EnergyTransfer           -- intersection info, direction to light source
    generateRay  :: RandomGen gen => gen -> brdf -> Intersection geom -> (Ray, gen) -- generate new ray reflected/refracted from the surface

data BRDFs = Diffuse EnergyTransfer
                deriving (Eq, Show)

transfer r g b = EnergyTrans( V3 r g b )

instance BRDF BRDFs a where
    evalBRDF _ Environment _ = undefined
    evalBRDF (Diffuse (EnergyTrans reflectivity)) (Hit _ _ inormal _) dir2light =
        EnergyTrans( cs' *^ reflectivity ) where
            cs  = dot (normalized inormal) (normalized dir2light)
            cs' = clamp 0 1 cs

    generateRay _ _ Environment = undefined
    generateRay gen (Diffuse reflectColor) (Hit _ ipoint inormal _) = (Ray (ipoint, normalize3 dir), gen'') where
        (ran_theta, gen') = next gen
        (ran_phi, gen'')  = next gen'

        -- surface normal
        V3 x y z = normalized inormal
        r     = sqrt (x*x + y*y + z*z)
        theta = acos (z/r)
        phi   = atan (y/x)

        dir = V3 (sin theta' * cos phi') (sin theta' * sin phi') (cos theta')
        theta' = (inRange ran_theta * 0.5 * pi) + theta
        phi'   = (inRange ran_phi * 2 * pi) + phi