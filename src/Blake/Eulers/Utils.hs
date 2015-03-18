module Blake.Eulers.Utils where

-- Multiplies the components of a 3-tuple
product3Tuple :: Num a => (a, a, a) -> a
product3Tuple (a, b, c) = a * b * c

-- Squares the given number
square :: Num a => a -> a
square n = n * n