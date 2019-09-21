{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

module BraKet where

import Data.Complex

import LinAlg
import Util

data Basis = Fock
    deriving (Show, Eq)

-- Ket is a quantum ket vector
-- 'a' is the label type, 'b' is the basis
-- ket type
data Ket c where
    Ket :: Basis -> [c] -> Ket c
    Nil :: Ket c
      deriving (Show, Eq)

data Bra c where
    Bra :: Basis -> [c] -> Bra c
    NilBra :: Bra c
      deriving (Show, Eq)

instance Dual (Ket (Complex Double)) (Bra (Complex Double)) where
    dual (Ket b cs) = Bra b $ conjugate <$> cs

instance Dual (Bra (Complex Double)) (Ket (Complex Double)) where
    dual (Bra b cs) = Ket b $ conjugate <$> cs

instance Functor Ket where
    fmap f (Ket b cs) = Ket b (f <$> cs)

instance Vector (Ket (Complex Double)) where
    -- Zero vector
    vzero = Nil
    -- Negation: just negate all coefficients
    vneg ket = negate <$> ket
    
    -- Addition: add coefficients element-wise
    k@(Ket _ _) <+> Nil = k
    Nil <+> k@(Ket _ _) = k
    (Ket b exp1) <+> (Ket _ exp2) = Ket b $ zipWith (+) 
            (padZeroN lendif21 exp1) (padZeroN lendif12 exp2)
      where 
        lendif12 = max 0 $ length exp1 - length exp2
        lendif21 = max 0 $ length exp2 - length exp1

    -- Scalar multiplication: multiply all coefficients
    a <**> ket = (*a) <$> ket

instance Hilbert (Ket (Complex Double)) where
    -- Inner product: Like l2 inner product; multiply coefficients 
    -- and sum
    (Ket _ coeffs1) <.> (Ket _ coeffs2) = sum $ zipWith (*) 
              (map conjugate $ padZeroN dif21 coeffs1) (padZeroN dif21 coeffs2)
      where 
        dif12 = max 0 $ length coeffs1 - length coeffs2
        dif21 = max 0 $ length coeffs2 - length coeffs1
