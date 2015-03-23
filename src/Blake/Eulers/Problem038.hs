module Blake.Eulers.Problem038 where

import Blake.Eulers.Utils (digits, fromDigits)
import Data.List (sort)

answer = last $ sort $ map (fromDigits . (\(i, n) -> productSpan i n)) $ filter (\ (i, n) -> isPandigital i n) [(i, n) | i <- [1..10000], n <- [1..9]]

-- Whether or not the given number is a pandigital number of `n`
isPandigital :: Integral a => a -> a -> Bool
isPandigital i n = length digs == 9 && sort digs == [1..9]
  where digs = productSpan i n

-- Multiplies the given number by every number from 1 to `n` and concats the products together
productSpan :: Integral a => a -> a -> [a]
productSpan i n = concat $ map (digits . (*) i) [1..n]