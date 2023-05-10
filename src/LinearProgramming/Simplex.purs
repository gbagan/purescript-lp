module LinearProgramming.Simplex (Error(..), simplex) where

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
import LinearProgramming.Class (class OrderedField)

data Error = Infeasible | NotBounded

derive instance Eq Error

instance Show Error where
  show Infeasible = "Infeasible"
  show NotBounded = "NotBounded"

empty :: forall a. Vector a
empty = V.fromArray []

-- matA is in canonical form
-- initxB is a feasible solution and initB a basis
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

-- matA is in canonical form
feasibleSolution :: forall a. OrderedField a => Matrix a -> Vector a -> Either Error { setB :: Array Int, xB :: Vector a }
feasibleSolution matA b =
  let
    m = M.nrows matA
    n = M.ncols matA    
    mat' = auxMatrix matA b
    obj' = V.fromArray $ replicate n zero <> replicate m (-one)
  in
  case simplex' mat' obj' (n .. (n + m - 1)) (abs <$> b) of
    Left e -> Left e
    Right x ->
      if V.dot obj' x < zero then
        Left Infeasible
      else
        let 
          setB' = 0 .. (n+m-1) # filter \j -> V.index x j > zero
          setB = take m $ setB' <> difference ((n-m) .. (n-1)) setB'
          xB = V.fromArray $ V.index x <$> setB
        in
          Right {setB, xB}

simplex :: forall a. OrderedField a => Matrix a -> Vector a -> Vector a -> Either Error (Vector a)
simplex matA b obj = do
  let m = M.nrows matA
  let n = M.ncols matA
  let matA' = canonicalForm matA
  {setB, xB} <- feasibleSolution matA' b
  let obj' = V.fromArray $ V.toArray obj <> replicate m zero
  sol <- simplex' matA' obj' setB xB
  pure $ V.fromArray $ take n $ V.toArray sol