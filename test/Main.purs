module Test.Main where

import Prelude
import Data.Either (Either(..))
import Data.Rational ((%))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Data.LinearAlgebra.Matrix as M
import Data.LinearAlgebra.Vector as V
import LinearProgramming.Simplex (Error(..), simplex)
import LinearProgramming.Program (Program(..), Objective(..), (++), (+-), (**), (&<=&), (&==&), solve)
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
    describe "simplex" do
      it "simplex 1" do
        let a = M.fromArray 3 4 [ [2, 4, 5, 7]
                                , [1, 1, 2, 2]
                                , [1, 2, 3, 3]
                                ] <#> (_ % 1)
        let b = V.fromArray [42, 17, 24] <#> (_ % 1)
        let obj = V.fromArray [7, 9, 18, 17] <#> (_ % 1)
        let sol = V.fromArray [3, 0, 7, 0] <#> (_ % 1)
        simplex a b obj `shouldEqual` Right sol

      it "simplex 2 (duplicate row)" do
        let a = M.fromArray 4 4 [ [2, 4, 5, 7]
                                , [1, 1, 2, 2]
                                , [1, 2, 3, 3]
                                , [2, 4, 5, 7]
                                ] <#> (_ % 1)
        let b = V.fromArray [42, 17, 24, 42] <#> (_ % 1)
        let obj = V.fromArray [7, 9, 18, 17] <#> (_ % 1)
        let sol = V.fromArray [3, 0, 7, 0] <#> (_ % 1)
        simplex a b obj `shouldEqual` Right sol


      it "simplex 3 (0 is not feasible)" do
        let a = M.fromArray 3 2 [ [3, 4]
                                , [-3, -4]
                                , [2, -1]
                                ] <#> (_ % 1)
        let b = V.fromArray [12, -12, 12] <#> (_ % 1)
        let obj = V.fromArray [-1, 2] <#> (_ % 1)
        let sol = V.fromArray [0, 3] <#> (_ % 1)
        simplex a b obj `shouldEqual` Right sol

      it "simplex 4 (no solution)" do
        let a = M.fromArray 2 2 [[1, 2], [-3, -1]] <#> (_ % 1)
        let b = V.fromArray [2, -10] <#> (_ % 1)
        let obj = V.fromArray [4, 3] <#> (_ % 1)
        simplex a b obj `shouldEqual` Left Infeasible

      it "simplex 5 (not bounded)" do
        let a = M.fromArray 2 2 [ [-1, -1]
                                , [0, 1]
                                ] <#> (_ % 1)
        let b = V.fromArray [-2, 3] <#> (_ % 1)
        let obj = V.fromArray [1, 2] <#> (_ % 1)
        simplex a b obj `shouldEqual` Left NotBounded

      it "simplex 5 (degenerated solution)" do
        let a = M.fromArray 2 3 [ [2, 0, 1]
                                , [1, 1, 1]
                                ] <#> (_ % 1)
        let b = V.fromArray [4, 3] <#> (_ % 1)
        let obj = V.fromArray [2%1, 0%1, 3%2]
        let sol = V.fromArray [1, 0, 2] <#> (_ % 1)
        simplex a b obj `shouldEqual` Right sol

      it "simplex 6 (infinite number of solutions)" do
        let a = M.fromArray 8 6 [ [5, 10, 10, 5, 1, 7]
                                , [-5, -10, -10, -5, -1, -7]
                                , [1, 0, 0, 0, 0, 0]
                                , [0, 1, 0, 0, 0, 0]
                                , [0, 0, 1, 0, 0, 0]
                                , [0, 0, 0, 1, 0, 0]
                                , [0, 0, 0, 0, 1, 0]
                                , [0, 0, 0, 0, 0, 1]
                                ] <#> (_ % 1)
        let b = V.fromArray [25, -25, 1, 1, 1, 1, 1, 1] <#> (_ % 1)
        let obj = V.fromArray [10, 8, 15, 4, 1, 5] <#> (_ % 1)
        (simplex a b obj <#> (_ `V.dot` obj)) `shouldEqual` Right (166%5)

      it "simplex 7 (0 is an optimal solution)" do
        let a = M.fromArray 2 6 [ [5, 10, 10, 5, 1, 7]
                                , [5, 10, 10, 5, 1, 7]
                                ] <#> (_ % 1)
        let b = V.fromArray [0, 0] <#> (_ % 1)
        let obj = V.fromArray [1, 1, 1, 1, 1, 1] <#> (_ % 1)
        let sol = V.fromArray [0, 0, 0, 0, 0, 0] <#> (_ % 1)
        simplex a b obj `shouldEqual` Right sol


    describe "program" do
      it "program 1" do
        let p = Program 
                { objective: Maximize $ 2 ** "x2" +- 1 ** "x1"
                , constraints: 
                    [ 2 ** "x1" ++ (-1) ** "x2" &<=& 12
                    , 3 ** "x1" ++ 4 ** "x2" &==& 12
                    ]
                } <#> (_ % 1)
        let sol = Map.fromFoldable [(Tuple "x1" $ 0 % 1),(Tuple "x2" $ 3 % 1)]
        solve p `shouldEqual` Right sol