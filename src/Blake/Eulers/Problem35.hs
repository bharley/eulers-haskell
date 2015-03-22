module Blake.Eulers.Problem35 where

import Blake.Eulers.Utils (digits, fromDigits, rotations)
import Data.Numbers.Primes (primes, isPrime)

answer = length $ filter isCircularPrime $ takeWhile (< 999999) primes

-- Note that this assumes the input is prime (to save another prime check calculation)
isCircularPrime :: Integral a => a -> Bool
isCircularPrime n = isCircularPrime' n
  where isCircularPrime' = (== l) . length . (filter isPrime) . init . intRotations
        l = (length $ digits $ n) - 1

-- Wraps the generic rotation function in a digit un/wrapping process
intRotations :: Integral a => a -> [a]
intRotations = (map fromDigits) . rotations . digits