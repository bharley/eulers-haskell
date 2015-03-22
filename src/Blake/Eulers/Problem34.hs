module Blake.Eulers.Problem34 where

import Blake.Eulers.Utils (digits, factorial)

answer = sum $ filter isCurious [3..500000]

-- Whether or not the given number is "curious" as dictated by the problem
isCurious :: Integral a => a -> Bool
isCurious n = n == facSum
  where facSum = sum $ map factorial $ digits n