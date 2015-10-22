{-# LANGUAGE FlexibleInstances #-}
module Light where

import qualified Prelude as P
import Numeric.Units.Dimensional.Prelude hiding ((-))
import Numeric.Units.Dimensional
import Math
import Linear
import Linear.Affine
import System.Random (RandomGen(..))
import Control.Applicative

type Color a        = V3 a
type LightIntensity = Color (LuminousIntensity Float)          -- R, G, B components of directional luminous intensity [candela]
type LightTrans     = Color (Dimensionless Float)              -- R, G, B coefficients of light transmitance [1/steradian]

envLightIntensity :: LightIntensity
envLightIntensity = zeroLightIntensity

zeroLightIntensity :: LightIntensity
zeroLightIntensity = V3 _0 _0 _0

attenuateWith :: LightIntensity -> LightTrans -> LightIntensity
attenuateWith i a = (*) <$> i <*> a

averageIntensity :: [LightIntensity] -> LightIntensity
averageIntensity [] = V3 _0 _0 _0
averageIntensity xs = (*invLength) <$> foldl1 (\a b -> (+)<$>a<*>b ) xs where
        invLength = _1 / (fromIntegral (length xs) *~ one)

class Shadow gen light where
    shadowRay :: RandomGen gen => gen -> light -> Coord3 -> (RaySegment, gen)
    eval      :: light -> UnitV3 -> LightIntensity                  -- dir2light

data Light = OmniLight (Coord3,             LuminousFlux Float) |   -- center, luminous flux [lumens]
             RectLight (Coord3, Vec3, Vec3, LuminousFlux Float)     -- center, side0, side1, luminous flux [lumens]
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

    eval (OmniLight (_, e)) _       = V3 (e*pi4) (e*pi4) (e*pi4) where pi4 = _1 / (_4*pi)
    eval (RectLight (_, _, _, e)) _ = V3 e e e -- TODO