{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

module QUtil where

import Data.Complex
import Data.List

import Test.QuickCheck
import Numeric.LinearAlgebra as L

import LinAlg
import Util
import BraKet
import Operators

-- Utility functions --
takeKet :: Element c => Int -> Ket c -> Ket c
takeKet n (Ket coeffs) = Ket $ L.subVector 0 n coeffs

-- Expected value of an operator O computed as Tr(rho <> O)
expectedValue :: MatOp -> [Complex Double] -> [Ket (Complex Double)] -> Complex Double
expectedValue op ps kets = trace $ op Prelude.<> densityMatrix ps kets

-- Commutator of two operators
commutator :: MatOp -> MatOp -> MatOp
commutator a b = (a Prelude.<> b) <~> (b Prelude.<> a)

-- normalization function
normalize :: Hilbert v => v -> v
normalize v = ((1.0 :+ 0.0) / (norm v :+ 0.0)) <**> v

-- sized vacuum Fock state
vacuum :: Int -> Ket (Complex Double)
vacuum n = Ket . L.fromList $ 1.0 : replicate (n-1) 0.0

-- n-photon Fock basis state
fockN :: Int -> Int -> Ket (Complex Double)
fockN m n = QUtil.normalize $ act (foldl1' (Prelude.<>) $ replicate n (create m)) (vacuum m)

-- Coherent state
coherent :: Int -> Complex Double -> Ket (Complex Double)
coherent m alpha = act (displacement m alpha) $ vacuum m
