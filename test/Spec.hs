{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck
import Test.QuickCheck.Gen

import Data.Complex

import LinAlg
import BraKet
import Operators
import QUtil

genSizedComplex :: Double -> Gen (Complex Double)
genSizedComplex x = (\z -> case z of
    value@(0.0 :+ 0.0) -> value
    _                  -> z / ((1.0/x * (magnitude z)) :+ 0.0))
    <$> (arbitrary :: Gen (Complex Double))

-- Coherent state is eigenstate of annihilation operator
-- within tolerance
-- need restriction on magnitude of alpha
-- as high magnitude alpha kills the expansion
prop_annihilate_eigenstate :: Property
prop_annihilate_eigenstate = 
    forAll (choose (50, 150)) $ \n -> 
        forAll (genSizedComplex 5.0) $ \alpha ->
            let state = takeKet n $ coherent alpha 
                in (norm ((act annihilate state <~> (alpha <**> state)))) <= 1e-3 * (fromIntegral n)
        
-- Displacing the vacuum creates a coherent state
prop_displaced_vacuum :: Complex Double -> Property
prop_displaced_vacuum alpha = norm (act (displacement 10 alpha) vacuum <~> (takeKet 10 $ coherent alpha)) <= 1e-3


return []
runTests = $quickCheckAll

main :: IO ()
main = do 
    result <- runTests
    return ()