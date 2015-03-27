module Blake.Eulers.Problem043 where

import Blake.Eulers.Utils (digits, fromDigits, sublist)
import Data.List (permutations)

answer = sum $ map fromDigits $ filter isSubstringDivisible domain

-- All permutations 0 to 9 digit pandigital numbers with any starting with '0' removed
domain :: Integral a => [[a]]
domain = filter ((/= 0) . head) $ permutations [0..9]

-- Determines whether or not the number has the substring divisibility property
isSubstringDivisible :: [Int] -> Bool
isSubstringDivisible dgts = s == 0
  where s = sum [rem (sublens i dgts) (divisors !! (i - 1)) | i <- [1..7]]

-- Grabs the element at list position `n` and the next 2 elements
sublens :: Integral a => a -> [a] -> a
sublens i = fromDigits . sublist i' j'
  where i' = fromIntegral i
        j' = fromIntegral (i + 3)

-- The numbers the substrings need to be evenly divisible by
divisors :: Integral a => [a]
divisors = [2, 3, 5, 7, 11, 13, 17]