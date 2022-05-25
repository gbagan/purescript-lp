module Test.Main where

import Prelude
import Data.Either (Either(..))
import Data.Rational ((%))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Data.LinearAlgebra.Matrix as M
import Data.LinearAlgebra.Vector as V
import Data.LinearProgramming.Simplex (Error(..), simplex)
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
        simplex a b obj `shouldEqual` Left NoSolution

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
