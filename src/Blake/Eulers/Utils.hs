module Blake.Eulers.Utils where

import Control.Arrow ((&&&))
import Data.List (group, sort, foldl')
import Data.Bits (testBit, bitSize)

-- http://stackoverflow.com/a/3963286
digits :: Integral x => x -> [x]
digits 0 = []
digits x = digits (x `div` 10) ++ [x `mod` 10]

-- http://stackoverflow.com/a/1918515/352967
fromDigits :: Integral a => [a] -> a
fromDigits = foldl addDigit 0
   where addDigit num d = 10 * num + d

-- Gets a slice of a list
sublist :: Int -> Int -> [a] -> [a]
sublist a b lst = take (b - a) . drop a $ lst

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

-- Easy peasy lemon squeezy
factorial :: Integral a => a -> a
factorial n = product [1..n]

-- You spin me right round baby
rotations :: Eq a => [a] -> [[a]]
rotations n = rotations' [n]
  where rotations' rots@(n':_)
          | (length rots) == (length n) = rots
          | otherwise                   = rotations' $ (last n' : init n') : rots

-- Determines if a number is a palindrome
palindrome :: Integral n => [n] -> Bool
palindrome []     = True -- Not sure about this case, but we shouldn't need it
palindrome (_:[]) = True
palindrome (x:xs) = if x == (last xs) then (palindrome $ init xs) else False

-- Infinite Fibonacci sequence
-- https://wiki.haskell.org/The_Fibonacci_sequence#Canonical_zipWith_implementation
--fibonacci :: Num a => [a]
--fibonacci = g (1,0)  where  g (a,b) = b : g (a+b,a)

fibonacci :: Int -> Integer
fibonacci n = snd . foldl' fib' (1, 0) . dropWhile not $
            [testBit n k | k <- let s = bitSize n in [s-1,s-2..0]]
          where
              fib' (f, g) p
                  | p         = (f*(f+2*g), ss)
                  | otherwise = (ss, g*(2*f-g))
                  where ss = f*f+g*g