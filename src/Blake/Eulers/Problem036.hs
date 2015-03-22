module Blake.Eulers.Problem036 where

import Blake.Eulers.Utils (palindrome)
import Data.Digits (digits)

answer = sum $ filter doublePalindrome [1..999999]

-- Whether or not the number and it's binary encoding are both palindromes
doublePalindrome :: Integral a => a -> Bool
doublePalindrome n = (palindrome n') && (palindrome bin)
  where n'  = digits 10 n
        bin = digits 2 n