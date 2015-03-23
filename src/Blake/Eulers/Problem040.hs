module Blake.Eulers.Problem040 where

import Blake.Eulers.Utils (digits, fromDigits)

answer = product $ map (fraction !!) [10^n - 1 | n <- [0..6]]

-- The fractional parts from the problem
fraction :: Integral a => [a]
fraction = concat $ map digits [1..200000] -- 1 -> 200,000 gives us over 1 million digits to play with