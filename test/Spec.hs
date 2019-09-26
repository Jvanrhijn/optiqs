{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

import Test.QuickCheck
import Test.QuickCheck.Gen
import qualified Data.Vector.Unboxed as U

import Data.Complex

import LinAlg
import BraKet
import Operators
import Util
import QUtil

instance Arbitrary (Ket (Complex Double)) where
    arbitrary = (Ket Fock . U.fromList) <$> arbitrary

genSizedComplex :: Double -> Gen (Complex Double)
genSizedComplex x = (\z -> case z of
    value@(0.0 :+ 0.0) -> value
    _                  -> z / ((1.0/x * (magnitude z)) :+ 0.0))
    <$> (arbitrary :: Gen (Complex Double))

-- test correctness of operator exponential
-- sqrt <n|z * exp(I)|n> == exp 1
-- operator exponential works up to 60 terms before
-- numerical issues arise and everything becomes NaN
prop_op_exp_constant :: Complex Double -> Property
prop_op_exp_constant z = forAll (choose (20, 50)) $ \n -> 
    forAll (choose (0, 100)) $ \m -> 
        let tolerance = (magnitude z)^(n + 2) / (fromIntegral $ factorial (n + 2))
        in abs ((norm $ act (expOp n (z <**> (Operator id))) (fockN m)) - magnitude (exp z)) <= 1.0

-- Test normalization of Kets
prop_normalization :: Ket (Complex Double) -> Property
prop_normalization ket@(Ket Fock cs) = not (U.null cs) ==>  (norm (normalize ket) - 1.0) <= 1e-10

-- Coherent state is eigenstate of annihilation operator
-- within tolerance
-- need restriction on magnitude of alpha
-- as high magnitude alpha kills the expansion
prop_annihilate_eigenstate :: Property
prop_annihilate_eigenstate = 
    forAll (choose (50, 150)) $ \n -> 
        forAll (genSizedComplex 5.0) $ \alpha ->
            let state = coherent n alpha 
                in (norm ((act annihilate state <~> (alpha <**> state)))) <= 1e-3 * (fromIntegral n)
        
-- Displacing the vacuum creates a coherent state
-- Too large a displacement is vulnerable to numerical
-- instability
prop_displaced_vacuum :: Property
prop_displaced_vacuum = forAll (choose (10, 50)) $ \n -> 
    forAll (genSizedComplex 1.0) $ \alpha ->
        norm (act (displacement (n+1) alpha) vacuum <~> (coherent n alpha)) <= 1e-3 * (fromIntegral n)


return []
runTests = $quickCheckAll

main :: IO ()
main = do 
    result <- runTests
    return ()