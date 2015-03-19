module Blake.Eulers.Problem14 where

import Data.List (sort, nub)

answer = (last . sort . nub) [(length . collatz) n | n <- [1..999999]]

-- Calculates the Collatz Sequence for n
collatz :: Integral a => a -> [a]
collatz n
  | n == 1         = [n]
  | n `mod` 2 == 0 = n : collatz (n `div` 2)
  | otherwise      = n : collatz ((3 * n) + 1)