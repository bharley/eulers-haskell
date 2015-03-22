module Blake.Eulers.Problem014 where

import Data.List (sortBy, nub)
import Data.Function (on)

--answer = (last . sort . nub) [(n, (length . collatz) n) | n <- [1..999999]]
answer = (fst . last . sortBy (compare `on` snd)) [(n, (length . collatz) n) | n <- [1..999999]]

-- Calculates the Collatz Sequence for n
collatz :: Integral a => a -> [a]
collatz n
  | n == 1         = [n]
  | n `mod` 2 == 0 = n : collatz (n `div` 2)
  | otherwise      = n : collatz ((3 * n) + 1)