{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module BRDF where

import Geometry
import Light
import Math
import Linear

class BRDF brdf geom where
    evalBRDF :: brdf -> Intersection geom -> Normal -> Energy -> Energy  -- intersection info, direction to light source

data BRDFs = Diffuse Color deriving Eq

instance BRDF BRDFs a where
    evalBRDF _ Environment _ _ = undefined
    evalBRDF (Diffuse reflectColor) (Hit _ _ inormal _) dir2light (Energy fromLight) =
        Energy( cs' *^ (fromLight * reflectColor) ) where
            cs  = dot (normalized inormal) (normalized dir2light)
            cs' = clamp 0 1 cs