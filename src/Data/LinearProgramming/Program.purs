module Data.LinearProgramming.Program where

import Prelude
import Prim hiding (Constraint)
import Data.Array ((!!), filter, length, mapWithIndex, replicate, updateAtIndices)
import Data.Either (Either(..))
import Data.Set as Set
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (fromMaybe, fromJust)
import Data.Tuple (Tuple(..))
import Data.LinearAlgebra.Matrix as M
import Data.LinearAlgebra.Vector as V
import Data.LinearProgramming.Class (class OrderedField)
import Data.LinearProgramming.Simplex (Error, simplex)
import Partial.Unsafe (unsafePartial)

data Atom v c = Atom v c

derive instance Functor (Atom v)


data Expr v c = Expr (Array (Atom v c))

instance Semigroup (Expr v c) where
  append (Expr e1) (Expr e2) = Expr (e1 <> e2)

derive instance Functor (Expr v)


data Objective v c = Minimize (Expr v c) | Maximize (Expr v c)

derive instance Functor (Objective v)


data Constraint v c
  = LessOrEqual (Expr v c) c
  | GreaterOrEqual (Expr v c) c
  | Equal (Expr v c) c

derive instance Functor (Constraint v)

infix 4 LessOrEqual as &<=&
infix 4 GreaterOrEqual as &>=&
infix 4 Equal as &==&

atom :: forall v c. c -> v -> Expr v c
atom coeff var = Expr [Atom var coeff]
infix 6 atom as **

infixr 5 append as ++

data Program v c = Program 
  { objective :: Objective v c
  , constraints :: Array (Constraint v c)
  }

derive instance Functor (Program v)

arrayOfVars :: forall v c. Ord v => Program v c -> Array v
arrayOfVars (Program {objective, constraints})
  = Set.toUnfoldable $ Set.union objVars cstrVars 
  where
    objVars = case objective of 
      Maximize expr -> exprVars expr
      Minimize expr -> exprVars expr
    exprVars (Expr e) = Set.fromFoldable $ e <#> \(Atom v c) -> v
    cstrVars = Set.unions (cstrVars' <$> constraints)
    cstrVars' (LessOrEqual e _) = exprVars e
    cstrVars' (GreaterOrEqual e _) = exprVars e
    cstrVars' (Equal e _) = exprVars e

solveLinearProgram :: forall v c. Ord v => OrderedField c =>   
                        Program v c -> Either Error (Map v c) 
solveLinearProgram program@(Program {objective, constraints}) = do
  sol <- simplex (M.fromArray n m matA) (V.fromArray b) (V.fromArray obj)
  pure $ 
    sol
      # V.toArray
      # mapWithIndex (\i c -> Tuple (unsafePartial $ fromJust $ vars !! i) c)
      # Map.fromFoldable
  where
  vars = arrayOfVars program
  m = length vars
  reverseVars = Map.fromFoldable $ vars # mapWithIndex (flip Tuple)
  matA = constraints >>= cstrmat
  b = constraints >>= cstrb
  n = length b
  obj = case objective of
    Maximize e -> exprToVec e
    Minimize e -> negate <$> exprToVec e 
  cstrmat (LessOrEqual e _)=  [exprToVec e]
  cstrmat (GreaterOrEqual e _) = [negate <$> exprToVec e]
  cstrmat (Equal e _) = [exprToVec e, negate <$> exprToVec e]
  cstrb :: Constraint v c -> Array c
  cstrb (LessOrEqual _ c) = [c]
  cstrb (GreaterOrEqual _ c) = [-c]
  cstrb (Equal _ c) = [c, -c]
  exprToVec (Expr e) = 
    let
      update = e <#> \(Atom v c) -> Tuple (fromMaybe (-1) $ Map.lookup v reverseVars) c
    in replicate m zero # updateAtIndices update