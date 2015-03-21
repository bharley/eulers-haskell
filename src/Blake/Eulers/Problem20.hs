module Blake.Eulers.Problem20 where

import Blake.Eulers.Utils (digits, factorial)

answer = (sum . digits . factorial) 100