module Blake.Eulers.Problem10 where

import Data.Numbers.Primes (primes)

-- Yay...
answer = sum $ takeWhile (< 2000000) primes