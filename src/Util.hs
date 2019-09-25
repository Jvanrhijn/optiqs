{-# LANGUAGE FlexibleContexts #-}
module Util where

import Data.Complex
import Data.List

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Generic.Mutable as M
import Numeric.FFT.Vector.Invertible

-- 2D fourier transform
-- fourier transform all rows, then all columns
fft2d :: [[Complex Double]] -> [[Complex Double]]
fft2d vs = transpose $ map (U.toList . run dft . U.fromList) $ 
    transpose $ map (U.toList . run idft . U.fromList) vs

-- general useful stuff

factorial :: Integral a => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

zeros :: Num a => [a]
zeros = 0 : zeros

padZeroN :: Num a => Int -> [a] -> [a]
padZeroN n xs = xs ++ replicate n 0

padZeroN' :: U.Unbox a => Num a => Int -> U.Vector a -> U.Vector a
padZeroN' n xs = xs U.++ U.replicate n 0

padZero :: Num a => [a] -> [a]
padZero = (++zeros)

complexToPlane :: [Complex Double] -> String
complexToPlane [] = ""
complexToPlane ((x :+ y):zs) = show x ++ " " ++ show y ++ "\n" ++ complexToPlane zs

-- for testing
myExpTerm :: Int -> Complex Double -> Complex Double
myExpTerm 0 _ = 1.0 :+ 0.0
myExpTerm n z = z / (fromIntegral n) * myExpTerm (n-1) z

myExp :: Int -> Complex Double -> Complex Double
myExp n z = sum $ [myExpTerm n z | n <- [0..n]]