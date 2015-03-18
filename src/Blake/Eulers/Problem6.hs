module Blake.Eulers.Problem6 where

range = [1..100]
answer = (squareOfSum range) - (sumOfSquares range)

-- The sum of the squares of the given range
sumOfSquares :: Integral a => [a] -> a
sumOfSquares = sum . (map square)

-- The square of the sum of the given range
squareOfSum :: Integral a => [a] -> a
squareOfSum = square . sum

-- Squares the given valuw
square :: Integral a => a -> a
square x = x * x