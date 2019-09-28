{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}

module BraKet where

import Data.Complex
import qualified Numeric.LinearAlgebra as L

import LinAlg
import Util

-- Ket is a ket vector
-- is the coefficient type
-- in which the Ket is written
newtype Ket c = Ket (L.Vector c)
      deriving (Show, Eq)

-- Function used to act on a Ket with an operator
act :: Operator -> Ket (Complex Double) -> Ket (Complex Double)
act op ket = Ket $ repr op L.#> getCoeffs ket

getCoeffs :: Ket c -> L.Vector c
getCoeffs (Ket cs) = cs

mapKet :: (L.Vector a -> L.Vector a) -> Ket a -> Ket a
mapKet f (Ket cs) = Ket $ f cs

instance Vector (Ket (Complex Double)) where
    -- Negation: just negate all coefficients
    vneg = mapKet negate
    -- Addition: add coefficients element-wise
    (Ket exp1) <+> (Ket exp2) = Ket $ exp1 + exp2
    -- Scalar multiplication: multiply all coefficients
    (<**>) a = mapKet (L.cmap (*a))

instance Hilbert (Ket (Complex Double)) where
    -- Inner product: Like l2 inner product; multiply coefficients 
    -- and sum
    (Ket coeffs1) <.> (Ket coeffs2) = coeffs1 L.<.> coeffs2

outerProduct :: Ket (Complex Double) -> Ket (Complex Double) 
                                     -> Operator
outerProduct (Ket cs1) (Ket cs2) = Operator $ cs1 `L.outer` cs2