module Wigner where

import Operators
import BraKet
import Util
import LinAlg
import Lib

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


-- test stuff

expDispFock :: Complex Double -> [Ket (Complex Double)] -> Complex Double
expDispFock alpha states = expectDisp 20 alpha ps basis states
  where
    ps = replicate (length states) (1.0 / (fromIntegral $ length states)) :: [Double]
    basis = map fockN [0..10]

getPlane :: Double -> Double -> Double -> [Complex Double]
getPlane dz zMin zMax = (:+) <$> range <*> range 
  where
    range = [zMin, zMin+dz..zMax]

computeExpD :: Double -> (Double, Double) -> [Ket (Complex Double)] -> [Complex Double]
computeExpD dz (zMin, zMax) states = (flip expDispFock states) <$> plane
  where plane = getPlane dz zMin zMax

writeComplex :: FilePath -> [Complex Double] -> IO ()
writeComplex fpath = writeFile fpath . complexToPlane

