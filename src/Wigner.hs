module Wigner where

import Operators
import BraKet
import Util
import LinAlg
import QUtil 

import Data.Complex
import Data.List

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
-- tThen normalize 
wt2d:: [[Complex Double]] -> [[Complex Double]]
wt2d = map ((/pi^2)<$>) . transpose . map (U.toList . run dft . U.fromList) .
    transpose . map (U.toList . run idft . U.fromList)
