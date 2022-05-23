module Data.LinearProgramming.Simplex where

import Data.Maybe (Maybe(..))
import Prelude
import LinearAlgebra.Matrix as M
import LinearAlgebra.Matrix (Matrix)
import LinearAlgebra.Vector as V
import LinearAlgebra.Vector (Vector)
import Data.LinearProgramming.Class (class OrderedField)

canonicalForm :: forall a. Ring a => Matrix a -> Matrix a
canonicalForm m = M.fromFunction r (r + c) fAug
  where
  r = M.nrows m
  c = M.ncols m
  fAug i j
    | j < c = M.elem i j m
    | i == j - c = one
    | otherwise = zero


simplex :: forall a. OrderedField a => Matrix a -> Vector a -> Vector a -> Maybe (Vector a)
simplex m b obj = Nothing
  where
  m'= canonicalForm m
  