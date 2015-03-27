module Blake.Eulers.Problem048 where

import Blake.Eulers.Utils (digits, fromDigits, sublist)

answer = fromDigits $ sublist (length n' - 10) (length n') n'
  where n' = digits $ sum $ [n^n | n <- [1..1000]]