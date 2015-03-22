module Blake.Eulers.Problem005 where

-- This is a naive answer that is quite inefficient
step = 38 -- This seems like a decent value to step by
answer = (+ step) $ last $ takeWhile (not . evenlyDivisibleByOneThroughTwenty) [step * x | x <- [1..]]

-- Now that's a function name
evenlyDivisibleByOneThroughTwenty :: Integral x => x -> Bool
evenlyDivisibleByOneThroughTwenty x = (== 0) $ sum $ map (mod x) [20,19,18,17,16,15,14,13,12,11]