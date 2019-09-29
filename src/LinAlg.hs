{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

module LinAlg where

import qualified Numeric.LinearAlgebra as L

import Data.Complex
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
    norm v = sqrt . magnitude $ v <.> v

-- Data structure representing a generic linear operator
-- in some representation
newtype Operator r = Operator {
    repr :: r 
} 

instance Functor Operator where
    fmap f (Operator r) = Operator $ f r

instance Applicative Operator where
    pure = Operator
    (Operator f) <*> op = f <$> op

-- Type alias for matrix representation of operator
type MatOp = Operator (L.Matrix (Complex Double))

-- Allow operators to be composed
instance Semigroup r => Semigroup (Operator r) where
    (Operator r1) <> (Operator r2) = Operator (r1 <> r2)

-- Operators are vectors
instance Vector MatOp where
    o1 <+> o2 = Operator (repr o1 + repr o2)
    a <**> o = L.cmap (*a) <$> o
    vneg o = negate <$> o

-- Trace of an operator 
trace :: MatOp -> Complex Double
trace = L.sumElements . L.takeDiag . repr