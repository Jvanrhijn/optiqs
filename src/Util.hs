module Util where

import Data.Complex

factorial :: Integral a => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

zeros :: Num a => [a]
zeros = 0 : zeros

padZeroN :: Num a => Int -> [a] -> [a]
padZeroN n xs = xs ++ replicate n 0

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