module Blake.Eulers.Problem004 where

import Data.List
import Blake.Eulers.Utils (digits, palindrome)

-- The answer
answer = last $ sort $ filter palindrome $ map digits $ nub threeDigitProducts


-- Unique 
threeDigitProducts = [x * y | x <- [900..999], y <- [900..999]] -- We're only going from 900->999 because it's likely the answer is in this set