module Util where

factorial :: Integral a => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

zeros :: Num a => [a]
zeros = 0 : zeros

padZeroN :: Num a => Int -> [a] -> [a]
padZeroN n xs = xs ++ replicate n 0

padZero :: Num a => [a] -> [a]
padZero = (++zeros)
