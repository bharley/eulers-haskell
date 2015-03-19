module Blake.Eulers.Problem4 where

import Data.List
import Blake.Eulers.Utils (digits)

-- The answer
answer = last $ sort $ filter palindrome $ map digits $ nub threeDigitProducts


-- Unique 
threeDigitProducts = [x * y | x <- [900..999], y <- [900..999]] -- We're only going from 900->999 because it's likely the answer is in this set


-- Determines if a number is a palindrome
palindrome :: Integral n => [n] -> Bool
palindrome []     = True -- Not sure about this case, but we shouldn't need it
palindrome (_:[]) = True
palindrome (x:xs) = if x == (last xs) then (palindrome $ init xs) else False
