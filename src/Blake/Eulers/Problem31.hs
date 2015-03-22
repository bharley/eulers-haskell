module Blake.Eulers.Problem31 where

-- Brute force it, baby (the +2 comes from the note above the definition of "combinations")
answer = ((+2) . length) combinations

-- The value 
values = [1, 2, 5, 10, 20, 50, 100]

-- We ommit the 2x£1 and 1x£2 cases to cut down on the number of possibilities
combinations = [[a, b, c, d, e, f, g] | b <- [0..100],
                                        c <- [0..40],
                                        d <- [0..20],
                                        e <- [0..10],
                                        f <- [0..4],
                                        g <- [0..1],
                                        let a = 200 - ((values !! 1) * b +
                                                       (values !! 2) * c +
                                                       (values !! 3) * d +
                                                       (values !! 4) * e +
                                                       (values !! 5) * f +
                                                       (values !! 6) * g),
                                        a >= 0]