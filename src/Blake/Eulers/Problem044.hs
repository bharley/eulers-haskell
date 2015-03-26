module Blake.Eulers.Problem044 where

import Data.List (sort)
import Blake.Eulers.Utils (takeWhileInclusive)

answer = head $ sort $ concat [[d | j <- takeWhile (< k) pentagonals,
                                    let d = abs (k - j),
                                    isPentagonal (k + j),
                                    isPentagonal d]
                                  | k <- take 10000 pentagonals]

-- Infinite list of pentagonal numbers
pentagonals :: (Fractional a, Enum a) => [a]
pentagonals = [pentagonal i | i <- [1..]]

-- The pentagonal number P(n)
pentagonal :: Fractional a => a -> a
pentagonal n = (n * (3 * n - 1)) / 2

-- Whether or not the given number is pentagonal
isPentagonal :: (RealFrac a, Floating a) => a -> Bool
isPentagonal p = not $ hasDecimal n
  where n = (1 / 6) * (sqrt (24 * p + 1) + 1)

-- Determines whether or not the given number has an decimal components
hasDecimal :: RealFrac a => a -> Bool
hasDecimal n = floor n /= ceiling n
