module Main where

import Wigner
import BraKet
import Operators
import Util
import QUtil
import LinAlg

import Data.Complex

main :: IO ()
main = do
    writeComplex "expd" $ computeExpD dZ (-bound, bound) states
    writeComplex "plane" $ getPlane dZ (-bound) bound

states :: [Ket (Complex Double)]
states = [fockN 0]

bound :: Double
bound = 3

dZ :: Double
dZ = 0.1

expDispFock :: Complex Double -> [Ket (Complex Double)] -> Complex Double
expDispFock alpha states = expectDisp 10 alpha ps basis states
  where
    ps = replicate (length states) (1.0 / (fromIntegral $ length states)) :: [Double]
    basis = map fockN [0..2]

getPlane :: Double -> Double -> Double -> [Complex Double]
getPlane dz zMin zMax = (:+) <$> range <*> range 
  where
    range = [zMin, zMin+dz..zMax]

computeExpD :: Double -> (Double, Double) -> [Ket (Complex Double)] -> [Complex Double]
computeExpD dz (zMin, zMax) states = (flip expDispFock states) <$> plane
  where plane = getPlane dz zMin zMax

writeComplex :: FilePath -> [Complex Double] -> IO ()
writeComplex fpath = writeFile fpath . complexToPlane