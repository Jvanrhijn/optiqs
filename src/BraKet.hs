{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}

module BraKet where

import Data.Complex
import qualified Data.Vector.Unboxed as U

import LinAlg
import Util

data Basis = Fock
    deriving (Show, Eq)

-- Ket is a quantum ket vector
-- 'c' is the label type
-- Basis determines the basis in
-- in which the Ket is written
data Ket c where
    Ket :: Basis -> U.Vector c -> Ket c
      deriving (Show, Eq)

getCoeffs :: Ket c -> U.Vector c
getCoeffs (Ket _ cs) = cs

mapKet :: (U.Unbox a, U.Unbox b) => (a -> b) -> Ket a -> Ket b
mapKet f (Ket basis cs) = Ket basis $ U.map f cs

instance Vector (Ket (Complex Double)) where
    -- Zero vector
    vzero = Ket Fock U.empty
    -- Negation: just negate all coefficients
    vneg = mapKet negate
    
    -- Addition: add coefficients element-wise
    (Ket b exp1) <+> (Ket _ exp2) = Ket b $ U.zipWith (+) 
            (padZeroN' lendif21 exp1) (padZeroN' lendif12 exp2)
      where
        lendif12 = max 0 $ U.length exp1 - U.length exp2
        lendif21 = max 0 $ U.length exp2 - U.length exp1

    -- Scalar multiplication: multiply all coefficients
    (<**>) a = mapKet (*a)

instance Hilbert (Ket (Complex Double)) where
    -- Inner product: Like l2 inner product; multiply coefficients 
    -- and sum
    (Ket _ coeffs1) <.> (Ket _ coeffs2) = U.sum $ U.zipWith (*) 
              (U.map conjugate $ padZeroN' dif21 coeffs1) (padZeroN' dif21 coeffs2)
      where 
        dif12 = max 0 $ U.length coeffs1 - U.length coeffs2
        dif21 = max 0 $ U.length coeffs2 - U.length coeffs1

outerProduct :: Ket (Complex Double) -> Ket (Complex Double) 
                                     -> Operator (Ket (Complex Double))
outerProduct ket bra = Operator $ \operand -> (ket <.> operand) <**> ket