module Blake.Eulers.Problem039 where

import Data.List (sortBy)

answer = fst $ last $ sortBy compareLengths $ map (\set -> (head set, length set)) $ filter notEmpty candidates
  where notEmpty = (> 0) . length
        compareLengths (_, a) (_, b) = a `compare` b

candidates = [[p | a <- [1 .. div p 2],
                   b <- [1 .. div p 2],
                   b > a,
                   let c = p - a - b,
                   c > a,
                   c > b,
                   c^2 == a^2 + b^2]
                 | p <- [4..1000]]