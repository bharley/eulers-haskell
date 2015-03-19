module Blake.Eulers.Problem9 where

import Blake.Eulers.Utils (square, product3Tuple, takeWhileInclusive)

answer = product3Tuple $ last $ takeWhileInclusive (not . isPythagoreanTriplet) (possibleTuples 1000)

-- A list of 3-tuples satisfying `a + b + c = n`
possibleTuples n = [(a, b, c) | a <- [1..n], b <- [1..n], let c = n - (a + b), (a + b + c) == n]

-- Determines whether or not the given 3-tuple is a Pythagorean triplet
isPythagoreanTriplet :: Integral i => (i, i, i) -> Bool
isPythagoreanTriplet (a, b, c) = (a2 + b2) == c2
  where a2 = (square a)
        b2 = (square b)
        c2 = (square c)
