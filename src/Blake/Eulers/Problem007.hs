module Blake.Eulers.Problem007 where

import Data.Numbers.Primes (primes)

-- I'm using a nice primes library, so this is pretty much cheating :)
answer = primes !! 10000 -- 10,001st prime with a 0 index