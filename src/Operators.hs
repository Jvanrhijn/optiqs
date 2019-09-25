module Operators where

import Data.Complex

import BraKet
import LinAlg
import Util

-- Creation and annihilation operator definitions --

create :: Operator (Ket (Complex Double))
create = Operator (\state -> case state of 
      -- Any operator acting on vzero returns vzero
      Nil -> Nil   
      (Ket Fock cs) -> Ket Fock $ (zipWith (*) 
            sqrts newCoeffs)
        where
          sqrts = map ((:+0.0) . sqrt) [0.0..numStates+1]
          numStates = fromIntegral $ length cs :: Double
          newCoeffs = (1.0 :+ 0.0) : cs 
        )

-- TODO: have lowering operator act correctly on the vacuum
annihilate :: Operator (Ket (Complex Double))
annihilate = Operator (\ket -> case ket of 
      -- Any operator acting on vzero returns vzero
      Nil -> Nil   
      -- General case
      (Ket Fock cs) -> Ket Fock $ (zipWith (*) sqrts newCoeffs)
        where
          sqrts = map (sqrt . (:+0.0) . (+1.0)) [0.0..numStates-1]
          numStates = fromIntegral $ length cs :: Double
          newCoeffs = tail cs
      )

-- Number operator definition: n = conj(a) <> a
number :: Operator (Ket (Complex Double))
number = create <> annihilate

-- Quadratures
quadX1 = (0.5 :+ 0.0) <**> (annihilate <+> create)
quadX2 = ((1.0 :+ 0.0)/(0.0 :+ 2.0)) <**> (annihilate <~> create)

-- Density matrix operator
densityMatrix :: [Double] -> [Ket (Complex Double)] -> Operator (Ket (Complex Double))
densityMatrix ps kets = foldl1 (<+>) (zipWith (<**>) ((:+0.0) <$> ps) outProds)
  where
    outProds = map (\state -> outerProduct state state) kets

-- Operator exponential
expOp :: Vector v => Int -> Operator v -> Operator v
expOp nterms op = Operator id <+> 
      (foldl1 (<+>) [((1.0 :+ 0.0) / (fromIntegral $ factorial k))
                          <**> (mconcat $ replicate k op) | k <- [1..nterms]])

-- Displacement operator
displacement :: Int -> Complex Double -> Operator (Ket (Complex Double))
displacement n alpha = expOp n (alpha <**> create <~> ((conjugate alpha) <**> annihilate))

-- Squeezing operator
squeeze :: Int -> Complex Double -> Operator (Ket (Complex Double))
squeeze n z = expOp n ((0.5 :+ 0.0) <**> (conjugate z <**> asq <~> (z <**> asq')))
  where
    asq' = create <> create
    asq = annihilate <> annihilate 