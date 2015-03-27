module Blake.Eulers.Problem045 where

import Blake.Eulers.Problem044 (hasDecimal, isPentagonal)
import Blake.Eulers.Utils (takeWhileInclusive)

answer = last $ filter (\h -> isPentagonal h && isTriangle h) $ take 30000 hexagonals

-- Whether or not the given number is triangular
isTriangle :: (RealFrac a, Floating a) => a -> Bool
isTriangle t = not $ hasDecimal n
  where n = 0.5 * (sqrt (8 * t + 1) - 1)

-- Infinite list of hexagonal numbers
hexagonals :: (Num a, Enum a) => [a]
hexagonals = [hexagonal i | i <- [1..]]

-- The hexagonal number T(n)
hexagonal :: Num a => a -> a
hexagonal n = n * (2 * n - 1)