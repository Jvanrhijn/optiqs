module Wigner where

import Operators
import BraKet
import Util
import LinAlg
import QUtil 

import Data.Complex

-- Expected value of displacement operator
-- for a given density matrix
-- Params: 
-- - Number of terms to use for displacement operator expansion
-- - Displacement parameter
-- - Probabilities per state
-- - Basis in which to evaluate trace
-- - States for which to compute density matrix
expectDisp :: Int -> Complex Double 
                -> [Double] -> [Ket (Complex Double)] 
                -> [Ket (Complex Double)] -> Complex Double
expectDisp n alpha ps basis states = expectedValue ps basis (displacement n alpha) states