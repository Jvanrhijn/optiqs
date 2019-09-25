{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

module LinAlg where

import Data.Complex

class Vector v where
    -- vector addition
    (<+>) :: v -> v -> v 
    -- scalar multiplication
    (<**>) :: Complex Double -> v -> v
    -- zero vector
    vzero :: v
    -- additive inverse 
    vneg :: v -> v
    (<~>) :: v -> v -> v
    -- vector subtraction for convenience
    u <~> v = u <+> (vneg v)

class Vector v => Hilbert v where
    -- Inner product
    (<.>) :: v -> v -> Complex Double
    -- Vector norm, default definition generates norm
    -- from inner product
    norm :: v -> Double
    norm v = magnitude $ sqrt $ v <.> v

-- Type class representing mathematical objects
-- with a dual representation, for instance
-- the bra <-> ket duality.
class Dual a b where
    dual :: a -> b

-- Data structure represnting a generic linear operator
-- mapping an object into the same space, hence it wraps
-- an endofunction.
data Operator v = Operator {
    act :: v -> v
}

instance Semigroup (Operator v) where
    (Operator f1) <> (Operator f2) = Operator (f1 . f2)

instance Monoid (Operator v) where
    mempty = Operator id

instance Vector v => Vector (Operator v) where
    o1 <+> o2 = Operator (\x -> (act o1 x) <+> (act o2 x))
    a <**> o = Operator (\x -> a <**> act o x)
    vzero = Operator (\_ -> vzero)
    vneg o = Operator (\x -> vneg $ act o x)

-- Trace of an operator in some basis
trace :: Hilbert v => [v] -> Operator v -> Complex Double
trace basis op = sum $ map (\vec -> vec <.> (act op vec)) basis

