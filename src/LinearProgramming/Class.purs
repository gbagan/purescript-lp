module LinearProgramming.Class where

import Prelude
import Data.Rational (Rational)

-- | an ordered field is a field with a total order satisfying the following laws:
-- | if a < b then a + c < b + c
-- | if a > 0 and b > 0 then  a * b > 0 
class (Field a, Ord a) <= OrderedField a

instance OrderedField Number
instance OrderedField Rational
