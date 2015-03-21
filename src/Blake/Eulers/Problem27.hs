module Blake.Eulers.Problem27 where

import Data.Numbers.Primes (isPrime)
import Data.List (sortBy)
import Data.Ord (comparing)

answer = snd $ last $ sortBy (comparing $ fst) $ concat [[(numberOfConsecutivePrimes a b, a * b) | a <- [-999..999]] | b <- [-999..999]]

-- The number of consecutive primes this quadratic will generate
numberOfConsecutivePrimes :: Integral a => a -> a -> Int
numberOfConsecutivePrimes a b = length $ takeWhile (isPrime . quad) [0..]
  where quad n = (n^2) + (a * n) + b