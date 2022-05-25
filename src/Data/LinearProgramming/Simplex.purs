module Data.LinearProgramming.Simplex (Error(..), simplex) where

import Prelude
import Data.Array ((..), (!!), difference, filter, find, replicate, sort, take, updateAtIndices, zip)
import Data.Either (Either(..))
import Data.Foldable (minimumBy)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ord (abs, signum)
import Data.Tuple (Tuple(..))
import Data.LinearAlgebra.Matrix as M
import Data.LinearAlgebra.Matrix (Matrix)
import Data.LinearAlgebra.Vector as V
import Data.LinearAlgebra.Vector (Vector)
import Data.LinearProgramming.Class (class OrderedField)

data Error = NoSolution | NotBounded

derive instance Eq Error

instance Show Error where
  show NoSolution = "NoSolution"
  show NotBounded = "NotBounded"

empty :: forall a. Vector a
empty = V.fromArray []


simplex' :: forall a. OrderedField a => Matrix a -> Vector a -> Array Int -> Vector a -> Either Error (Vector a)
simplex' matA obj initB initxB = go initB initN initxB
  where
  m = M.nrows matA
  n = M.ncols matA
  initN = difference (0..(n-1)) initB
  go setB setN xB =
    let
      optB = V.fromArray $ V.index obj <$> setB
      matB =  M.fromColumns m m (M.column matA <$> setB)
      y = fromMaybe empty $ M.solveLinearSystem' (M.transpose matB) optB
    in
      case sort setN # find \j -> V.index obj j - y `V.dot` M.column matA j > zero of
        Nothing -> Right $ V.fromArray $ replicate n zero # updateAtIndices (zip setB (V.toArray xB))
        Just j0 ->
          let
            d = fromMaybe empty $ M.solveLinearSystem' matB (M.column matA j0)
          in
            case 0 .. (m - 1)
                # filter (\j -> V.index d j > zero)
                # minimumBy (comparing \j -> V.index xB j / V.index d j)
                >>= \j -> Tuple (V.index xB j / V.index d j) <$> setB !! j of
              Nothing -> Left NotBounded
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


auxMatrix :: forall a. OrderedField a => Matrix a -> Vector a -> Matrix a
auxMatrix a b =  M.fromFunction r (r + c) fAug
  where
  r = M.nrows a
  c = M.ncols a
  fAug i j
    | j < c = M.index a i j
    | i == j - c = signum (V.index b i)
    | otherwise = zero

canonicalForm :: forall a. Ring a => Matrix a -> Matrix a
canonicalForm mat = M.fromFunction r (r + c) fAug
  where
  r = M.nrows mat
  c = M.ncols mat
  fAug i j
    | j < c = M.index mat i j
    | i == j - c = one
    | otherwise = zero

simplex :: forall a. OrderedField a => Matrix a -> Vector a -> Vector a -> Either Error (Vector a)
simplex matA b obj =
  let
    m = M.nrows matA
    n = M.ncols matA
    matA' = canonicalForm matA
  -- search for a feasible solution    
    matAux = auxMatrix matA' b
    objAux = V.fromArray $ replicate (n+m) zero <> replicate m (-one)
  in
  case simplex' matAux objAux ((n + m) .. (n + m + m - 1)) (abs <$> b) of
    Left e -> Left e
    Right x ->    
      if V.dot objAux x < zero then
        Left NoSolution
      else
        let 
          initB' = 0 .. (n+m-1) # filter \j -> V.index x j > zero
          initB = take m $ initB' <> difference (n .. (n+m-1)) initB'
          xB = V.fromArray $ V.index x <$> initB
          obj' = V.fromArray $ V.toArray obj <> replicate m zero
        in
          simplex' matA' obj' initB xB <#> (V.fromArray <<< take n <<< V.toArray)
