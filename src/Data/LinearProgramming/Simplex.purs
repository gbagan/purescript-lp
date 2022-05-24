module Data.LinearProgramming.Simplex (simplex) where

import Prelude
import Data.Array ((..), (!!), filter, find, replicate, sort, updateAtIndices, zip)
import Data.Foldable (minimumBy)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Data.LinearAlgebra.Matrix as M
import Data.LinearAlgebra.Matrix (Matrix)
import Data.LinearAlgebra.Vector as V
import Data.LinearAlgebra.Vector (Vector)
import Data.LinearProgramming.Class (class OrderedField)

empty :: forall a. Vector a
empty = V.fromArray []

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
simplex matA b obj = go initB initN b
  where
  m = M.nrows matA
  n = M.ncols matA
  matA' = canonicalForm matA
  obj' = V.fromArray $ V.toArray obj <> replicate m zero
  initB = n .. (m + n - 1)
  initN = 0 .. (n - 1)

  go setB setN xB =
    let
      optB = V.fromArray $ V.index obj' <$> setB
      matB =  M.fromColumns m m (M.column matA' <$> setB)
      y = fromMaybe empty $ M.solveLinearSystem' (M.transpose matB) optB
    in
      case sort setN # find \j -> V.index obj' j - y `V.dot` M.column matA' j > zero of
        Nothing -> Just $ V.fromArray $ replicate n zero # updateAtIndices (zip setB (V.toArray xB))
        Just j0 ->
          let
            d = fromMaybe empty $ M.solveLinearSystem' matB (M.column matA' j0)
          in
            case 0 .. (m - 1)
                # filter (\j -> V.index d j > zero)
                # minimumBy (comparing \j -> V.index xB j / V.index d j)
                >>= \j -> Tuple (V.index xB j / V.index d j) <$> setB !! j of
              Nothing -> Nothing
              Just (Tuple tmax i0) ->
                let
                  setB' = setB <#> \v -> if v == i0 then j0 else v
                  setN' = setN <#> \v -> if v == j0 then i0 else v
                  xB' =
                    xB
                      # V.mapWithIndex \j v ->
                          if setB !! j == Just i0 then
                            tmax
                          else
                            v - tmax * V.index d j
                in
                  go setB' setN' xB'
