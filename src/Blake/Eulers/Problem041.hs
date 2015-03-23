module Blake.Eulers.Problem041 where

import Blake.Eulers.Utils (digits)
import Data.Numbers.Primes (primes)
import Data.List (sort)

-- This solution isn't very efficient as it took 4179.52 seconds to execute (over an hour)
answer = last $ sort $ filter isPandigital $ takeWhile ((<= 9) . length . digits) primes

-- Whether or not the given number is a pandigital number of `n`
isPandigital :: Int -> Bool
isPandigital n = sort digs == [1 .. length digs]
  where digs = digits n