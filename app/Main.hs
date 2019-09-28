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

main :: IO ()
main = do
    let expd = computeExpD dim ps dZ (-bound, bound) states
    let n = round . sqrt . fromIntegral . length $ expd
    -- Wigner transform the expected D
    writeComplex "expd" . mconcat . wt2d . chunksOf n $ expd 
    writeComplex "plane" $ getPlane dZ (-bound) bound

-- dimension of vector space to work in
dim = 50
-- classical probabilities of mixed state
ps = [1.0]
-- number of steps in the complex plane to use
-- computation scales as O(nsteps^2)
nsteps = 20
-- bounds on the complex plane
-- for some reason the GC freaks out over a bound of 5.0 ¯\_(ツ)_/¯
bound = 5.1

-- step size in complex plane
dZ = 2.0 * bound / fromIntegral nsteps

-- list of states to use
-- really a list of functions, I use applicative to 
-- convert to a list of sized state vectors
states :: [Int -> Ket (Complex Double)]
-- vacuum 
states = [vacuum]
-- one photon Fock
--states = [flip fockN 1]
-- Schrodinger cat
--states = [\dim -> coherent dim (-2.0) <+> coherent dim 2.0]
-- Mixed schrodinger cat
--states = [flip coherent ((-2.0) :+ 0.0), flip coherent (2.0 :+ 0.0)]

-- Construct the complex plane from a range and step size
getPlane :: Double -> Double -> Double -> [Complex Double]
getPlane dz zMin zMax = (:+) <$> range <*> range 
  where
    range = [zMin, zMin+dz..zMax]

-- Compute the expected value of the displacement operator
-- over the complex plane
computeExpD :: Int -> [Complex Double] -> Double -> (Double, Double) 
                   -> [Int -> Ket (Complex Double)] -> [Complex Double]
computeExpD dim ps dz (zMin, zMax) states = (\z -> expectDisp dim z ps (states <*> [dim])) <$> plane
  where plane = getPlane dz zMin zMax

-- Write complex array to file
writeComplex :: FilePath -> [Complex Double] -> IO ()
writeComplex fpath = writeFile fpath . complexToPlane

-- Make list of complex numbers easy to parse by python
complexToPlane :: [Complex Double] -> String
complexToPlane [] = ""
complexToPlane ((x :+ y):zs) = show x ++ " " ++ show y ++ "\n" ++ complexToPlane zs