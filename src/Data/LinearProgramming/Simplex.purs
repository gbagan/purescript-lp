module Data.LinearProgramming.Simplex where

import Prelude
import Data.Array ((..), (!!), find, replicate, sort, updateAtIndices, zip)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.LinearAlgebra.Matrix as M
import Data.LinearAlgebra.Matrix (Matrix)
import Data.LinearAlgebra.Vector as V
import Data.LinearAlgebra.Vector (Vector)
import Data.LinearProgramming.Class (class OrderedField)

canonicalForm :: forall a. Ring a => Matrix a -> Matrix a
canonicalForm mat = M.fromFunction r (r + c) fAug
  where
  r = M.nrows mat
  c = M.ncols mat
  fAug i j
    | j < c = M.index mat i j
    | i == j - c = one
    | otherwise = zero


simplex :: forall a. OrderedField a => Matrix a -> Vector a -> Vector a -> Maybe (Vector a)
simplex matA b obj = go initB initN initX
  where
  m = M.nrows matA
  n = M.ncols matA
  matA'= canonicalForm matA
  obj' = V.fromArray $ V.toArray obj <> replicate m zero
  initB = n..(m+n-1)
  initN = 0..(n-1)
  initX = b ------

  go setB setN xB =
      let
        optB = V.fromArray $ V.index obj' <$> setB
        matB =  M.fromColumns m m (M.column matA' <$> setB)
        y = fromMaybe (V.fromArray []) $ M.solveLinearSystem' (M.transpose matB) optB
      in
        case sort setN # find \j -> V.index obj' j - y `V.dot` M.column matA' j > zero of
          Nothing -> Just $ V.fromArray $ replicate n zero # updateAtIndices (zip setB (V.toArray xB))
          Just j0 ->
            let 
              d = fromMaybe (V.fromArray []) $ M.solveLinearSystem' matB (M.column matA j0)
              mi0 = Just 0
            -- max t (>0) tel que xB - t.d >=0
            {-
            sortante = -1
            min = 1000
            for j in range(m):
                if V.index d j > 0
                    coeff = V.index vecX j / V.index d j
                    if coeff < min:
                        min = coeff
                        sortante = Ba[j]
            -}
            in
            case mi0 of
              Nothing -> Nothing
              Just i0 ->
                let 
                  tmax = zero
                  setB' = setB <#> \v -> if v == i0 then j0 else v 
                  setN' = setN <#> \v -> if v == j0 then i0 else v 
                  xB' = xB # V.mapWithIndex \j v -> 
                                if setB !! j == Just i0 then
                                  tmax
                                else
                                  v - tmax * V.index d j
                in
                  go setB' setN' xB'