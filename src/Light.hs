module Light where

import qualified Prelude as P
import Numeric.Units.Dimensional.Prelude hiding ((-))
import Math
import Linear
import Linear.Affine
import System.Random (RandomGen(..))
import Control.Applicative

-- |Light description
type Color a        = V3 a
type LightIntensity = Color (LuminousIntensity Float)          -- R, G, B components of directional luminous intensity [candela]
type LightTrans     = Color (Dimensionless Float)              -- R, G, B coefficients of light transmitance [1/steradian]

-- |Light & shadow interface
class Shadow gen light where
    shadowRay :: RandomGen gen => gen -> light -> Coord3 -> (RaySegment, gen)
    eval      :: light -> UnitV3 -> LightIntensity                  -- dir2light

-- |Supported light types
data Light = OmniLight (Coord3,             LuminousFlux Float) |   -- center, luminous flux [lumens]
             RectLight (Coord3, Vec3, Vec3, LuminousFlux Float)     -- center, side0, side1, luminous flux [lumens]
                deriving Show

-- |Implementation of lights
instance Shadow gen Light where
    shadowRay gen (OmniLight (pos, _)) point' =
        (RaySeg (Ray (point', dir), dist), gen) where
            vec2light = pos .-. point'
            dir       = normalize3 vec2light
            dist      = norm vec2light

    shadowRay gen (RectLight (ptC, side0, side1, _)) point' =
        (RaySeg (Ray (point', dir), dist), gen'') where

            vec2light       = pt .-. point'
            dir             = normalize3 vec2light
            pt              = ptC .+^ (vpt0 P.+ vpt1)
            dist            = norm vec2light
            (vpt0, vpt1)    = (side0 ^* sampleX, side1 ^* sampleY) :: (V3 Float, V3 Float)
            (ran_x, gen')   = next gen
            (ran_y, gen'')  = next gen'
            (sampleX, sampleY) = (inRange gen ran_x P.- 0.5, inRange gen' ran_y P.- 0.5) :: (Float, Float)

    eval (OmniLight (_, e)) _               = V3 (e*pi4) (e*pi4) (e*pi4) where pi4 = _1 / (_4*pi)
    eval (RectLight (_, side0, side1, e)) _ = V3 (e*k) (e*k) (e*k) where
        k       = _1 / (_4*pi*surface)                   -- double sided
        surface = (norm side0 P.* norm side1) *~ one     -- TODO [m^2]


-----------------------------------------------------------------------------------------------------------------------
-- Utilities

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

transfer :: Float -> Float -> Float -> LightTrans
transfer r g b = V3 (r *~one) (g *~one) (b *~one)