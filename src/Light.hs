{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Light where

import qualified Prelude as P
import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional
import Math
import Linear
import Linear.Affine
import System.Random (RandomGen(..))

type Color a      = Point V3 a   -- absolute point in color space
type ColorTrans a = V3 a         -- delta energy - like vector in color space

envLightIntensity :: LightIntensity
envLightIntensity = P( V3 _0 _0 _0 )

zeroLightIntensity :: LightIntensity
zeroLightIntensity = P( V3 _0 _0 _0 )

type LightIntensity = Color (LuminousIntensity Float)               -- R, G, B components of directional energy
type Albedo         = ColorTrans (Dimensionless Float)              -- R, G, B coefficients of light transmissance

attenuateWith :: LightIntensity -> Albedo -> LightIntensity
attenuateWith (P(V3 er eg eb)) (V3 tr tg tb) = coord (er * tr) (eg * tg) (eb * tb)

-- averageLightIntensity :: [LightIntensity] -> LightIntensity
-- averageLightIntensity xs = P( sum' ^* (1/count) ) where
--    (sum', count) = foldl (\(acc,cnt) e -> (acc + e, cnt+1)) (V3 0 0 0, 0) . map energyColor $ xs
--    energyColor (P c) = c

class Shadow gen light where
    shadowRay :: RandomGen gen => gen -> light -> Coord3 -> (RaySegment, gen)
    eval      :: light -> LightIntensity

data Light = OmniLight (Coord3,             LuminousFlux Float) |   -- center, integral luminous flux [lumens]
             RectLight (Coord3, Vec3, Vec3, LuminousFlux Float)     -- center, side0, side1, integral luminous flux [lumens]
                deriving Show

instance Shadow gen Light where
    shadowRay gen (OmniLight (pos, _)) point' = (RaySeg (Ray (point', dir), dist), gen) where
        vec2light = pos .-. point'
        dir       = normalize3 vec2light
        dist      = norm vec2light

    shadowRay gen (RectLight (ptC, side0, side1, _)) point' = (RaySeg (Ray (point', dir), dist), gen'') where
        vec2light       = pt .-. point'
        dir             = normalize3 vec2light
        pt              = ptC .+^ (vpt0 P.+ vpt1)
        dist            = norm vec2light
        (vpt0, vpt1)    = (side0 ^* sampleX, side1 ^* sampleY) :: (V3 Float, V3 Float)
        (ran_x, gen')   = next gen
        (ran_y, gen'')  = next gen'
        (sampleX, sampleY) = (inRange gen ran_x P.- 0.5, inRange gen' ran_y P.- 0.5) :: (Float, Float)

    eval (OmniLight (_, e))       = coord e e e -- TODO /4*pi ?
    eval (RectLight (_, _, _, e)) = coord e e e -- TODO