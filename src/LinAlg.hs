{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

module LinAlg where

import qualified Numeric.LinearAlgebra as L

import Data.Complex
import Util
import Data.List

class Vector v where
    -- vector addition
    (<+>) :: v -> v -> v 
    -- scalar multiplication
    (<**>) :: Complex Double -> v -> v
    -- additive inverse 
    vneg :: v -> v
    (<~>) :: v -> v -> v
    -- vector subtraction for convenience
    u <~> v = u <+> vneg v

class Vector v => Hilbert v where
    -- Inner product
    (<.>) :: v -> v -> Complex Double
    -- Vector norm, default definition generates norm
    -- from inner product
    norm :: v -> Double
    norm v = magnitude $ sqrt $ v <.> v

-- Data structure representing a generic linear operator
-- in a specific basis (we use the Fock basis here)
newtype Operator = Operator {
    repr :: L.Matrix (Complex Double)
} 

-- Allow operators to be composed
instance Semigroup Operator where
    (Operator r1) <> (Operator r2) = Operator (r1 L.<> r2)

-- Operators are vectors
instance Vector Operator where
    o1 <+> o2 = Operator (repr o1 + repr o2)
    a <**> o = Operator (L.cmap (*a) $ repr o)
    vneg o = Operator $ negate $ repr o

-- Trace of an operator 
trace :: Operator -> Complex Double
trace = L.sumElements . L.takeDiag . repr