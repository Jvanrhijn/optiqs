{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

import Test.QuickCheck
import Test.QuickCheck.Gen

import Data.Complex

import qualified Numeric.LinearAlgebra as L

import LinAlg
import BraKet
import Operators
import Util
import QUtil

instance Arbitrary (Ket (Complex Double)) where
    arbitrary = Ket . L.fromList <$> arbitrary

genSizedComplex :: Double -> Gen (Complex Double)
genSizedComplex x = (\z -> case z of
    value@(0.0 :+ 0.0) -> value
    _                  -> z / (1.0/x * magnitude z :+ 0.0))
    <$> (arbitrary :: Gen (Complex Double))

-- Coherent state is eigenstate of annihilation operator
-- within tolerance
-- need restriction on magnitude of alpha
-- as high magnitude alpha kills the expansion
prop_annihilate_eigenstate :: Property
prop_annihilate_eigenstate = 
    forAll (choose (1, 10)) $ \n -> 
        forAll (genSizedComplex 1.0) $ \alpha ->
            let state = coherent n alpha 
                in norm (act (annihilate n) state <~> (alpha <**> state)) <= 1e-3 * fromIntegral n
        

return []
runTests = $quickCheckAll

main :: IO ()
main = do 
    result <- runTests
    return ()