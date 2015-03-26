module Blake.Eulers.Problem053 where

import Blake.Eulers.Utils (factorial)

answer = length $ filter (> 1000000) [n `choose` r | n <- [1..100], r <- [1..n]]

-- Good ol' combinatorics
choose :: Integral a => a -> a -> a
choose n r = div n' (r' * nr')
  where n'  = factorial n
        r'  = factorial r
        nr' = factorial (n - r)