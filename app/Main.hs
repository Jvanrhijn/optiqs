{-# LANGUAGE FlexibleContexts #-}

module Main where

import Wigner
import BraKet
import Operators
import Util
import QUtil
import LinAlg

import Data.Complex
import Data.List.Split

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Generic.Mutable as M
import Numeric.FFT.Vector.Invertible

main :: IO ()
main = do
    --let expd = computeExpD dZ (-bound, bound) states
    --let n = round $ sqrt $ fromIntegral $ length expd
    --let expd2d = chunksOf n expd
    --writeComplex "expd" $ mconcat $ fft2d expd2d
    writeComplex "expd" $ computeExpD dZ (-bound, bound) states
    writeComplex "plane" $ getPlane dZ (-bound) bound

dim :: Int
dim = 10

states :: [Ket (Complex Double)]
states = [coherent dim 1.0]

basis :: [Ket (Complex Double)]
basis = map (fockN dim) [0, 1]

bound :: Double
bound = 3.0

dZ :: Double
dZ = 0.1

nterms :: Int
nterms = 50

expDispFock :: Complex Double -> [Ket (Complex Double)] -> Complex Double
expDispFock alpha states = expectDisp nterms alpha ps basis states
  where
    ps = replicate (length states) $ 1.0 / fromIntegral (length states) :: [Double]

getPlane :: Double -> Double -> Double -> [Complex Double]
getPlane dz zMin zMax = (:+) <$> range <*> range 
  where
    range = [zMin, zMin+dz..zMax]

computeExpD :: Double -> (Double, Double) -> [Ket (Complex Double)] -> [Complex Double]
computeExpD dz (zMin, zMax) states = (flip expDispFock states) <$> plane
  where plane = getPlane dz zMin zMax

writeComplex :: FilePath -> [Complex Double] -> IO ()
writeComplex fpath = writeFile fpath . complexToPlane