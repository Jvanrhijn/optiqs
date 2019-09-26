module Operators where

import Data.Complex
import Data.List (foldl1')

import BraKet
import LinAlg
import Util

import qualified Data.Vector.Unboxed as U

-- Creation and annihilation operator definitions --

create :: Operator (Ket (Complex Double))
create = Operator fun
    where
      fun (Ket Fock cs) = Ket Fock $ (U.zipWith (*) sqrts newCoeffs)
        where
          sqrts = U.generate (numStates + 2) ((:+0.0) . sqrt . fromIntegral)
          numStates = U.length cs 
          newCoeffs = U.cons (1.0 :+ 0.0) cs 

-- TODO: have lowering operator act correctly on the vacuum
annihilate :: Operator (Ket (Complex Double))
annihilate = Operator fun
    where
      fun (Ket Fock cs) = Ket Fock $ (U.zipWith (*) sqrts newCoeffs)
        where
          sqrts = U.generate numStates ((:+0.0) . sqrt . (+1.0) . fromIntegral)
          numStates = U.length cs 
          newCoeffs = U.tail cs

-- Number operator definition: n = conj(a) <> a
number :: Operator (Ket (Complex Double))
number = create <> annihilate

-- Quadratures
quadX1 = (0.5 :+ 0.0) <**> (annihilate <+> create)
quadX2 = ((1.0 :+ 0.0)/(0.0 :+ 2.0)) <**> (annihilate <~> create)

-- Density matrix operator
densityMatrix :: [Double] -> [Ket (Complex Double)] -> Operator (Ket (Complex Double))
densityMatrix ps kets = foldl1' (<+>) (zipWith (<**>) ((:+0.0) <$> ps) outProds)
  where
    outProds = map (\state -> outerProduct state state) kets

-- Displacement operator
displacement :: Int -> Complex Double -> Operator (Ket (Complex Double))
displacement n alpha = expOp n (alpha <**> create <~> ((conjugate alpha) <**> annihilate))

-- Squeezing operator
squeeze :: Int -> Complex Double -> Operator (Ket (Complex Double))
squeeze n z = expOp n ((0.5 :+ 0.0) <**> (conjugate z <**> asq <~> (z <**> asq')))
  where
    asq' = create <> create
    asq = annihilate <> annihilate 