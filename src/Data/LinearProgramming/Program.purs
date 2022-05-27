module Data.LinearProgramming.Program
  ( (&<=&)
  , (&==&)
  , (&>=&)
  , (**)
  , (++)
  , (+-)
  , Constraint(..)
  , Expr
  , Objective(..)
  , Program(..)
  , solve
  , atom
  , exprSub
  )
  where

import Prelude
import Prim hiding (Constraint)
import Data.Array ((!!), length, mapWithIndex, replicate, updateAtIndices)
import Data.Bifunctor (lmap)
import Data.Either (Either)
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

data Expr v c = Expr (Map v c)

instance (Ord v, Semiring c) => Semigroup (Expr v c) where
  append (Expr e1) (Expr e2) = Expr (Map.unionWith (+) e1 e2)

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
atom coeff var = Expr (Map.singleton var coeff)
infix 6 atom as **

infixl 5 append as ++

exprSub :: forall v c. Ord v => Ring c => Expr v c -> Expr v c -> Expr v c
exprSub e1 e2 = e1 ++ ((-one) <$> e2)

infixl 5 exprSub as +-

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
    exprVars (Expr e) = Map.keys e
    cstrVars = Set.unions (cstrVars' <$> constraints)
    cstrVars' (LessOrEqual e _) = exprVars e
    cstrVars' (GreaterOrEqual e _) = exprVars e
    cstrVars' (Equal e _) = exprVars e

solve :: forall v c. Ord v => OrderedField c =>   
            Program v c -> Either Error (Map v c) 
solve program@(Program {objective, constraints}) = do
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
  cstrb (LessOrEqual _ c) = [c]
  cstrb (GreaterOrEqual _ c) = [-c]
  cstrb (Equal _ c) = [c, -c]
  exprToVec (Expr e) = 
    let
      update :: Array _
      update = Map.toUnfoldable e <#> lmap \v -> fromMaybe (-1) (Map.lookup v reverseVars)
    in
      replicate m zero # updateAtIndices update