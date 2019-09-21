{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

import Data.Complex
import Test.QuickCheck

factorial :: Integral a => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

zeros :: Num a => [a]
zeros = 0 : zeros

padZeroN :: Num a => Int -> [a] -> [a]
padZeroN n xs = xs ++ replicate n 0

padZero :: Num a => [a] -> [a]
padZero = (++zeros)

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
    u <~> v = u <+> (vneg v)

class Vector v => Hilbert v where
    -- Inner product
    (<.>) :: v -> v -> Complex Double
    -- Vector norm, default definition generates norm
    -- from inner product
    norm :: v -> Double
    norm v = magnitude $ sqrt $ v <.> v

class Dual a b where
    dual :: a -> b

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

data Basis = Fock
    deriving (Show, Eq)

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

data LadderOperator = Raise | Lower
    deriving (Show, Eq)

data Operator v = Operator {
    act :: (v -> v)
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

expectedValue :: Hilbert v => Operator v -> v -> Complex Double
expectedValue op vec = vec <.> (act op vec)

commutator :: Vector v => Operator v -> Operator v -> Operator v
commutator a b = a <> b <~> b <> a

normalize :: Hilbert v => v -> v
normalize v = ((1.0 :+ 0.0) / (norm v :+ 0.0)) <**> v

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

-- Utility functions --

takeKet :: Int -> Ket c -> Ket c
takeKet n (Ket basis coeffs) = Ket basis $ take n coeffs

-- vacuum Fock state
vacuum :: Ket (Complex Double)
vacuum = Ket Fock [1.0]

-- n-photon Fock basis state
fockN :: Int -> Ket (Complex Double)
fockN n = normalize $ act ((mconcat . replicate n) create) vacuum 

-- Coherent state
coherent :: Complex Double -> Ket (Complex Double)
coherent alpha = (exp $ - (alpha * conjugate alpha) / 2.0) <**> (Ket Fock $ zipWith (/) 
                     (map (alpha**) $ fromIntegral <$> [0..]) 
                     (map (sqrt . fromIntegral . factorial) [0..]))

-- test stuff

k1 :: Ket (Complex Double)
k1 = Ket Fock $ (:+0.0) <$> [0.0..4.0]
