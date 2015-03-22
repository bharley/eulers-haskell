module Blake.Eulers.Problem020 where

import Blake.Eulers.Utils (digits, factorial)

answer = (sum . digits . factorial) 100