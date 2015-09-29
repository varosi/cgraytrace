module Tests where
import Test.QuickCheck
import Math

prop_clamp0 :: Int -> Bool
prop_clamp0 a = res >= 0 && res <= 1 where res = clamp 0 1 a

prop_clamp1, prop_clamp2, prop_clamp3 :: Bool
prop_clamp1 = clamp 0 1 0.5 == 0.5
prop_clamp2 = clamp 0 1 (-10) == 0
prop_clamp3 = clamp 0 1 10 == 1

main = do
        quickCheck prop_clamp0
        mapM_ quickCheck [prop_clamp1, prop_clamp2, prop_clamp3]
