module Blake.Eulers.Problem024 where

import Blake.Eulers.Utils (fibonacci, digits, takeWhileInclusive)

answer = snd $ last $ takeWhileInclusive ((< 1000) . length . digits . fst) [(fibonacci i, i) | i <- [1..]]