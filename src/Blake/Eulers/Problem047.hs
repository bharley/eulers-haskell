module Blake.Eulers.Problem047 where

import Blake.Eulers.Utils (takeWhileInclusive)
import Data.Numbers.Primes (primeFactors)
import Data.List (nub)

answer = last $ takeWhileInclusive ((== False) . fourConsecutive) [i | i <- [1..]]

-- Determines whether or not this number and the next three numbers have 4 distinct, prime factors
fourConsecutive :: Integral a => a -> Bool
fourConsecutive n = and $ map factorProperty [n .. n + 3]

-- Whether or not the given number has 4 distinct prime factors
factorProperty :: Integral a => a -> Bool
factorProperty = (== 4) . length . nub . primeFactors
