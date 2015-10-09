module Tests where
import Test.QuickCheck
import Test.Hspec
import Math

import GHC.Word
import Scene
import Geometry
import Linear
import Linear.Affine
import Light
import Material
import BRDF
import Debug.Trace
--import Raytracer

import System.Random.TF.Gen (seedTFGen)
import System.Random (RandomGen(..))

prop_clamp0 :: Int -> Bool
prop_clamp0 a = res >= 0 && res <= 1 where res = clamp 0 1 a

prop_clamp1, prop_clamp2, prop_clamp3 :: Bool
prop_clamp1 = clamp (0 :: Float) 1 0.5   == 0.5
prop_clamp2 = clamp (0 :: Float) 1 (-10) == 0
prop_clamp3 = clamp (0 :: Float) 1 10    == 1

prop_polar0 x y z = r >= 0                || (x == 0 && y == 0 && z == 0)   where SphereV r _ _ = toSpherical $ V3 x y z
prop_polar1 x y z = (t >= -pi && t <= pi) || (x == 0 && y == 0 && z == 0)   where SphereV _ t _ = toSpherical $ V3 x y z
prop_polar2 x y z = (distance v1 v2 <= 1e-6) || (dot v1 v1 <= 1e-3)  where
        v1 = V3 x y z
        v2 = fromSpherical . toSpherical $ v1

testScene :: Scene
testScene = Scene [sphere0, sphere1, plane0] [light0] where
        sphere0 = Entity (Sphere (P$V3 0 0 200) 20) (mkDiffuse 0.98 0 0)
        sphere1 = Entity (Sphere (P$V3 5 35 200) 25) (mkDiffuse 0 0.98 0)
        plane0  = Entity (Plane (normalize3(V3 0 1 (-0.5))) 100) (mkDiffuse 0.5 0.5 0.5)
        light0  = OmniLight (P$V3 (0) (0) 0, Brightness 1)

--testIt :: String
--testIt = show lightDist ++ "\n" ++ show hit ++ "\n" ++ show reflectedLight ++ "\n" ++ show shadowRay' where
--        testingScene = cornellScene
--        cameraRay''' = Ray (P$V3 (1) 0 (-80), normalize3 (V3 0 0 1))
--        hit@(Hit _ ipoint _ entity') = traceRay testingScene Nothing cameraRay'''
--        test  = pathTrace testingScene cameraRay'''
--        test1 = evalBRDF brdf hit dir2light . eval $ light
--        reflectedLight = traceRay testingScene (Just entity') shadowRay'
--        shadowRay'          = shadowRay light ipoint
--        Ray (_, dir2light)  = shadowRay'
--        Mat brdf  = enMaterial entity'
--        light     = head . scLights $ testingScene
--        lightDist = distance ipoint (lightPos light)

prop_inrange :: Word64 -> Word64 -> Word64 -> Word64 -> Bool
prop_inrange a b c d = (value >= 0) && (value <= 1) where
        (v, _) = next gen
        value  = inRange gen . fromIntegral $ v
        gen    = seedTFGen $ (a,b,c,d)

main :: IO ()
main = hspec $ do
        --describe "Math.clamp" $ do
            --it "should return between 0 and 1" $ do
                --quickCheck prop_clamp0
                -- mapM_ quickCheck [prop_clamp1, prop_clamp2, prop_clamp3]

        describe "Math.toSpherical" $ do
            it "should return positive radius" $ do
                property prop_polar0
            it "should return -pi..pi theta angle" $ do
                property prop_polar1

        describe "Math toSpherical and back" $ do
            it "should be equal from-to" $ do
                property prop_polar2

        describe "Math.inRange" $ do
            it "[0,1]" $ do
                property prop_inrange