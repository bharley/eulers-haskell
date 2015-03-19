module Blake.Eulers.Utils where

import Control.Arrow ((&&&))
import Data.List (group, sort)

-- http://stackoverflow.com/a/3963286
digits :: Integral x => x -> [x]
digits 0 = []
digits x = digits (x `div` 10) ++ [x `mod` 10]

-- Multiplies the components of a 3-tuple
product3Tuple :: Num a => (a, a, a) -> a
product3Tuple (a, b, c) = a * b * c

-- Squares the given number
square :: Num a => a -> a
square n = n * n

-- http://stackoverflow.com/a/22472610
takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive p (x:xs) = x : if p x then takeWhileInclusive p xs
                                         else []
-- http://stackoverflow.com/a/1480620/352967
divisors :: Int -> [Int]
divisors n = 1 : filter ((== 0) . rem n) [2 .. n `div` 2] ++ [n]

-- http://stackoverflow.com/a/10398874/352967
frequency :: Ord a => [a] -> [(Int, a)]
frequency = map (length &&& head) . group . sort