module Blake.Eulers.Problem052 where

import Blake.Eulers.Utils (digits, fromDigits, takeWhileInclusive)
import Data.List (sort)

answer = last $ takeWhileInclusive ((== False) . sameDigitMultiples) [1..]

-- Determines wether or not 2x..6x this number shares the same digits with the number itself
sameDigitMultiples :: Integral a => a -> Bool
sameDigitMultiples n = and $ map (== n') digitGroups
  where sortDigits  = sort . digits
        n'          = sortDigits n
        digitGroups = [sortDigits $ n * i | i <- [2..6]]