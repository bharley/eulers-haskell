module Blake.Eulers.Problem37 where

import Blake.Eulers.Utils (digits, fromDigits)
import Data.Numbers.Primes (primes, isPrime)

-- We have to chop off the first 4 results
answer = (sum . last . (take 5) . iterate tail) ans
  where ans = take 15 $ filter truncatablePrime primes

-- Determines if a number is a prime when truncated both directions
truncatablePrime :: Integral a => a -> Bool
truncatablePrime n = (left n') && (right n')
  where n'             = digits n
        left []        = True
        left (a:[])    = isPrime a
        left (a:rest)  = (isPrime $ fromDigits rest) && (left rest)
        right []       = True
        right (a:[])   = isPrime a
        right lst      = (isPrime $ fromDigits rest) && (right rest)
          where rest = init lst