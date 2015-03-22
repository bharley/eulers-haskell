module Blake.Eulers.Problem030 where

import Blake.Eulers.Utils (digits)

answer = sum $ filter isDigit5thPower [11..1000000]

isDigit5thPower :: Int -> Bool
isDigit5thPower n = (sum $ map (^5) $ digits n) == n