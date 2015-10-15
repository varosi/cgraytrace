{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Light where

import Math
import Linear
import Linear.Affine
import System.Random (RandomGen(..))

type Color = Point V3 Float   -- absolute point in color space

envEnergy :: Energy
envEnergy = Energy( P( V3 0 0 0 ) )

zeroEnergy :: Energy
zeroEnergy = Energy( P( V3 0 0 0 ) )

newtype Energy         = Energy Color           deriving (Num, Show, Eq)      -- R, G, B components of energy that we sense
newtype EnergyTransfer = EnergyTrans (V3 Float) deriving (Num, Show, Eq)      -- delta energy - like vector in color space

averageEnergy :: [Energy] -> Energy
averageEnergy xs = Energy . P $ sum' ^* (1/count) where
    (sum', count) = foldl (\(acc,cnt) e -> (acc + e, cnt+1)) (V3 0 0 0, 0) . map energyColor $ xs
    energyColor (Energy (P c)) = c

attenuateWith :: Energy -> EnergyTransfer -> Energy
attenuateWith (Energy(P e)) (EnergyTrans trans) = Energy( P( e * trans ))

class Shadow gen light where
    shadowRay :: RandomGen gen => gen -> light -> Coord3 -> (RaySegment, gen)
    eval      :: light -> Energy

newtype Brightness = Brightness Color deriving Show

data Light = OmniLight (Coord3, Brightness) |                   -- center, brightness
             RectLight (Coord3, Vec3, Vec3, Brightness)         -- center, side0, side1, brightness
                deriving Show

instance Shadow gen Light where
    shadowRay gen (OmniLight (pos, _)) point' = (RaySeg (Ray (point', dir), dist), gen) where
        vec2light = pos .-. point'
        dir       = normalize3 vec2light
        dist      = norm vec2light

    shadowRay gen (RectLight (ptC, side0, side1, _)) point' = (RaySeg (Ray (point', dir), dist), gen'') where
        vec2light       = pt .-. point'
        dir             = normalize3 vec2light
        pt              = ptC .+^ (vpt0 + vpt1)
        dist            = norm vec2light
        (vpt0, vpt1)    = (side0 ^* sampleX, side1 ^* sampleY)
        (ran_x, gen')   = next gen
        (ran_y, gen'')  = next gen'
        (sampleX, sampleY) = (inRange gen ran_x - 0.5, inRange gen' ran_y - 0.5) :: (Float, Float)

    eval (OmniLight (_, Brightness e))       = Energy e
    eval (RectLight (_, _, _, Brightness e)) = Energy e