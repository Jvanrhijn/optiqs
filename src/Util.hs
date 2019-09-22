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
