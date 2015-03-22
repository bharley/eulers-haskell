module Blake.Eulers.Problem023 where

import Data.List (permutations, sort)

answer = (sort $ permutations ['0'..'9']) !! 999999