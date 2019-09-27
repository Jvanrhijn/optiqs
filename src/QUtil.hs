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

import qualified Data.Vector.Unboxed as U

-- Utility functions --

takeKet :: U.Unbox c => Int -> Ket c -> Ket c
takeKet n (Ket basis coeffs) = Ket basis $ U.take n coeffs

expectedValue :: [Double] -> [Ket (Complex Double)] -> Operator (Ket (Complex Double)) 
                                        -> [Ket (Complex Double)]  -> Complex Double
--expectedValue op vec = vec <.> (act op vec)
expectedValue ps basis op vs = trace basis $ (densityMatrix ps vs) <> op

commutator :: Vector v => Operator v -> Operator v -> Operator v
commutator a b = a <> b <~> b <> a

normalize :: Hilbert v => v -> v
normalize v = ((1.0 :+ 0.0) / (norm v :+ 0.0)) <**> v

-- sized vacuum Fock state
vacuum :: Int -> Ket (Complex Double)
vacuum n = Ket Fock $ U.fromList $ [1.0] ++ replicate (n-1) 0.0

-- n-photon Fock basis state
fockN :: Int -> Int -> Ket (Complex Double)
fockN m n = normalize $ act ((mconcat . replicate n) create) (vacuum (m + 1))

-- Coherent state
coherent :: Int -> Complex Double -> Ket (Complex Double)
coherent m alpha = (exp $ - (alpha * conjugate alpha) / 2.0) <**> (Ket Fock $
                     U.fromList [alpha**(fromIntegral n) / (sqrt $ fromIntegral $ factorial n) | n <- [0..m]])


