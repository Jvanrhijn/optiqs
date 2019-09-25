{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

module QUtil where

import Data.Complex
import Test.QuickCheck

import LinAlg
import Util
import BraKet
import Operators

-- Utility functions --

takeKet :: Int -> Ket c -> Ket c
takeKet n (Ket basis coeffs) = Ket basis $ take n coeffs

expectedValue :: [Double] -> [Ket (Complex Double)] -> Operator (Ket (Complex Double)) 
                                        -> [Ket (Complex Double)]  -> Complex Double
--expectedValue op vec = vec <.> (act op vec)
expectedValue ps basis op vs = trace basis $ (densityMatrix ps vs) <> op

commutator :: Vector v => Operator v -> Operator v -> Operator v
commutator a b = a <> b <~> b <> a

normalize :: Hilbert v => v -> v
normalize v = ((1.0 :+ 0.0) / (norm v :+ 0.0)) <**> v

-- vacuum Fock state
vacuum :: Ket (Complex Double)
vacuum = Ket Fock [1.0]

-- n-photon Fock basis state
fockN :: Int -> Ket (Complex Double)
fockN n = normalize $ act ((mconcat . replicate n) create) vacuum 

-- Coherent state
coherent :: Complex Double -> Ket (Complex Double)
coherent alpha = (exp $ - (alpha * conjugate alpha) / 2.0) <**> (Ket Fock $
                     [alpha**(fromIntegral n) / (sqrt $ fromIntegral $ factorial n) | n <- [0..]])


