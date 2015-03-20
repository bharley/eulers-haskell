module Blake.Eulers.Problem17 where

answer = length $ filter wantChar $ concat [numberToEnglish x | x <- [1..1000]]

-- Some lists for making number words
numbers1t19 = ["One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten",
               "Eleven", "Twelve", "Thirteen", "Fourteen", "Fifteen", "Sixteen", "Seventeen", "Eighteen", "Nineteen"]
numbersDec = ["Twenty", "Thirty", "Forty", "Fifty", "Sixty", "Seventy", "Eighty", "Ninety"]

-- Filters out characters this algorithm doesn't care about
wantChar :: Char -> Bool
wantChar c = (c /= '-') && (c /= ' ')

-- A naive implementation of a function that converts a number into it's English word (only works 0 <= n <= 1000)
numberToEnglish :: Int -> String
numberToEnglish n
  | n < 20                       = numbers1t19 !! (n - 1)
  | n == 1000                    = "One Thousand"
  | (n < 100) && (rem n 10 == 0) = numbersDec !! ((n `div` 10) - 2)
  | rem n 100 == 0               = (numbers1t19 !! ((n `div` 100) - 1)) ++ " Hundred"
  | n > 100                      = (numberToEnglish ((n `div` 100) * 100)) ++ " and " ++ (numberToEnglish (rem n 100))
  | (n < 100) && (n >= 20)       = (numberToEnglish ((n `div` 10) * 10)) ++ "-" ++ (numberToEnglish (rem n 10))
  | otherwise                    = "UNKNOWN"