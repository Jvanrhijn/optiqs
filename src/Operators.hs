module Operators where

import Data.Complex
import Data.List (foldl1')

import BraKet
import LinAlg

import qualified Numeric.LinearAlgebra as L

-- Creation and annihilation operator definitions --

-- Creation operator in Fock basis
create :: Int -> MatOp
create n = Operator $ (L.><) n n elements
  where
    elements = [if i `elem` nonzeros then (sqrt .fromIntegral) (i `mod` n + 1) else 0 | i <- [0..n^2-1]]
    nonzeros = [i * n + i - 1 | i <- [0..n^2]]

-- Annihilation operator is just conjugate of creation
annihilate :: Int -> MatOp
annihilate n = L.tr <$> create n

-- Number operator definition: n = conj(a) <> a
number :: Int -> MatOp
number n = create n <> annihilate n

-- Density matrix operator
densityMatrix :: [Complex Double] -> [Ket (Complex Double)] -> MatOp
densityMatrix ps kets = foldl1 (<+>) $ zipWith (<**>) ps projectors
    where
      projectors = (\state -> outerProduct state state) <$> kets :: [MatOp]

-- Displacement operator
displacement :: Int -> Complex Double -> MatOp
displacement n alpha = L.expm <$> (conjugate alpha <**> create n
                                        <~> (alpha <**> annihilate n))

squeeze :: Int -> Complex Double -> MatOp
squeeze n z = L.expm <$> ((0.5 :+ 0.0) <**> (conjugate z <**> (annihilate n <> annihilate n) 
                                                  <~> (z <**> (create n <> create n))))