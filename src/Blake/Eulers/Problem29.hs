module Blake.Eulers.Problem29 where

import Data.List (nub)

answer = length $ nub $ concat [[a^b | a <- [2..100]] | b <- [2..100]]