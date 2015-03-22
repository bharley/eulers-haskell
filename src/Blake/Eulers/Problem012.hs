module Blake.Eulers.Problem012 where

import Data.Numbers.Primes (primeFactors)
import Blake.Eulers.Utils (takeWhileInclusive, frequency)

answer = last $ takeWhileInclusive ((<=500) . numberOfDivisors) [triangleNumber i | i <- [7..]]

-- Finds the requested triangle number
triangleNumber :: Int -> Int
triangleNumber n = (n * (n + 1)) `div` 2

-- Get the number of divisors
numberOfDivisors :: Int -> Int
numberOfDivisors = product . map ((+1) . fst) . frequency . primeFactors
