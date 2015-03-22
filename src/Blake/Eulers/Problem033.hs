module Blake.Eulers.Problem033 where

import Data.Ratio (denominator, (%), Ratio)

answer = denominator $ product $ map toRatio $ filter isCurious candidates

candidates = [(a, b, c, d) | a <- [1..9],
                             b <- [1..9],
                             c <- [1..9],
                             d <- [1..9],
                             a /= b,
                             c /= d,
                             (a == c) || (a ==d) || (b == c) || (b == d),
                             ((a * 10) + b) < ((c * 10) + d)]

toRatio :: Integral a => (a, a, a, a) -> Ratio a
toRatio (a, b, c, d) = num % den
  where num = (a * 10) + b
        den = (c * 10) + d

isCurious :: Integral a => (a, a, a, a) -> Bool
isCurious tup@(a, b, c, d) = (toRatio tup) == frac'
  where frac'
          | a == c = (b % d)
          | a == d = (b % c)
          | b == c = (a % d)
          | b == d = (a % c)