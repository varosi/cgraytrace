module Tests where
import Test.QuickCheck
import Math

import Scene
import Geometry
import Linear
import Linear.Affine
import Light
import Material
import BRDF
--import Raytracer

prop_clamp0 :: Int -> Bool
prop_clamp0 a = res >= 0 && res <= 1 where res = clamp 0 1 a

prop_clamp1, prop_clamp2, prop_clamp3 :: Bool
prop_clamp1 = clamp (0 :: Float) 1 0.5   == 0.5
prop_clamp2 = clamp (0 :: Float) 1 (-10) == 0
prop_clamp3 = clamp (0 :: Float) 1 10    == 1

testScene :: Scene
testScene = Scene [sphere0, sphere1, plane0] [light0] where
        sphere0 = Entity (Sphere (P$V3 0 0 200) 20) (Mat$Diffuse (V3 0.98 0 0))
        sphere1 = Entity (Sphere (P$V3 5 35 200) 25) (Mat$Diffuse (V3 0 0.98 0))
        plane0  = Entity (Plane (normalize3(V3 0 1 (-0.5))) 100) (Mat$Diffuse (V3 0.5 0.5 0.5))
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

main :: IO ()
main = do
        --putStrLn testIt

        putStrLn "QuickCheck ------\n"
        quickCheck prop_clamp0
        mapM_ quickCheck [prop_clamp1, prop_clamp2, prop_clamp3]
