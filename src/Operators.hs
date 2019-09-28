module Operators where

import Data.Complex
import Data.List (foldl1')

import BraKet
import LinAlg
import Util

import qualified Numeric.LinearAlgebra as L

-- Creation and annihilation operator definitions --

-- Creation operator in Fock basis
create :: Int -> Operator 
create n = Operator $ (L.><) n n elements
  where
    elements = [if i `elem` nonzeros then (sqrt .fromIntegral) (i `mod` n + 1) else 0 | i <- [0..n^2-1]]
    nonzeros = [i * n + i - 1 | i <- [0..n^2]]

-- Annihilation operator is just conjugate of creation
annihilate :: Int -> Operator 
annihilate = Operator . L.tr . repr . create

-- Number operator definition: n = conj(a) <> a
number :: Int -> Operator 
number n = create n <> annihilate n

-- Density matrix operator
densityMatrix :: [Complex Double] -> [Ket (Complex Double)] -> Operator
densityMatrix ps kets = Operator. sum . map repr $ zipWith (<**>) ps projectors
    where
      projectors = (\state -> outerProduct state state) <$> kets :: [Operator]

-- Displacement operator
displacement :: Int -> Complex Double -> Operator 
displacement n alpha = Operator $ 0 `seq` L.expm $ L.cmap (* conjugate alpha) (repr (create n)) 
                                - L.cmap (*alpha) (repr (annihilate n))