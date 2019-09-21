module Operators where

import Data.Complex

import BraKet
import LinAlg

data LadderOperator = Raise | Lower
    deriving (Show, Eq)

-- Creation and annihilation operator definitions --

create :: Operator (Ket (Complex Double))
create = Operator raisingFunc
    where
      -- Any operator acting on vzero returns vzero
      raisingFunc Nil = Nil   
      raisingFunc (Ket Fock cs) = Ket Fock $ (zipWith (*) 
            sqrts newCoeffs)
        where
          sqrts = map ((:+0.0) . sqrt) [0.0..numStates+1]
          numStates = fromIntegral $ length cs :: Double
          newCoeffs = (1.0 :+ 0.0) : cs 

-- TODO: have lowering operator act correctly on the vacuum
annihilate :: Operator (Ket (Complex Double))
annihilate = Operator loweringFunc
    where
      -- Any operator acting on vzero returns vzero
      loweringFunc Nil = Nil   
      loweringFunc (Ket Fock cs) = Ket Fock $ (zipWith (*) 
            sqrts newCoeffs)
        where
          sqrts = map (sqrt . (:+0.0) . (+1.0)) [0.0..numStates-1]
          numStates = fromIntegral $ length cs :: Double
          newCoeffs = tail cs

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
