module Wigner where

import Operators
import BraKet
import LinAlg
import QUtil 

import Data.Complex
import Data.List
import Data.List.Split

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Generic.Mutable as M
import Numeric.FFT.Vector.Invertible

-- Expected value of displacement operator
-- for a given density matrix
-- Params: 
-- - Number of terms to use for displacement operator expansion
-- - Displacement parameter
-- - Probabilities per state
-- - Basis in which to evaluate trace
-- - States for which to compute density matrix
expectDisp :: Int -> Complex Double 
                -> [Complex Double] -> [Ket (Complex Double)] -> Complex Double
expectDisp n alpha = expectedValue (displacement n alpha)

-- 2D Wigner transform
-- fourier transform all rows, then inversely FT all columns
-- Then normalize 
-- unfortunately this doesn't work that well, so manual transform is
-- implemented below with crude riemann integration
wt2d:: [[Complex Double]] -> [[Complex Double]]
wt2d = map ((/pi^2)<$>) . transpose . map (U.toList . run dft . U.fromList) .
    transpose . map (U.toList . run idft . U.fromList)

-- manual W transform
wignerTransform' :: Complex Double -> Double -> [Complex Double] -> [Complex Double] -> Complex Double
wignerTransform' alpha dbeta betas ds = sum $ map (*(dbeta :+ 0.0)) integrand
    where
        integrand = zipWith (*) ds $ (\beta -> exp $ alpha * conjugate beta - conjugate alpha * beta) <$> betas

wignerTransform :: [Complex Double] -> Double -> [Complex Double] -> [Complex Double] -> [Complex Double]
wignerTransform alphas dbeta betas ds = map (/pi^2) $ (\a -> wignerTransform' a dbeta betas ds) <$> alphas
