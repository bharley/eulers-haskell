module Blake.Eulers.Problem21 where

import Blake.Eulers.Utils (divisors)

answer = sum $ map snd $ filter (hasAmicable dmap) dmap
  where dmap = [(d n, n) | n <- [2..9999]]

-- A filtering function for kicking out non-amicable numbers
hasAmicable :: (Eq a) => [(a, a)] -> (a, a) -> Bool
hasAmicable [] _                     = False
hasAmicable ((d', i'):lst) (dval, i)
  | (d' == i) && (dval == i') && (i' /= i) = True
  | otherwise                              = hasAmicable lst (dval, i)

-- d(n) = sum of the proper divisors of `n`
d :: Int -> Int
d = sum . properDivisors
  where properDivisors = init . divisors -- A "proper divisor" is less than `n`, so we need to remove the last factor (which is `n` itself)
