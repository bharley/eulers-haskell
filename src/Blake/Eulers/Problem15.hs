module Blake.Eulers.Problem15 where

answer = paths 20

-- If we think of the cube as a rotated Pascal's Triangle, we see that the
-- vertices count up the number of "routes" available to that position from
-- the pathing requirements (down and right) which match with how Pascal's
-- Triangle is calculated (up and left)
paths n = row !! mid
  where n' = n * 2
        triangle = take (n' + 1) pascal
        row = last triangle
        mid
          | rem i 2 == 0 = j - 1
          | otherwise    = j
            where i = length row
                  j = i `div` 2

-- https://wiki.haskell.org/Blow_your_mind
pascal = iterate (\row -> zipWith (+) ([0] ++ row) (row ++ [0])) [1]