module Test.Main where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Rational ((%))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Data.LinearAlgebra.Matrix as M
import Data.LinearAlgebra.Vector as V
import Data.LinearProgramming.Simplex (simplex)
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
        simplex a b obj `shouldEqual` Just sol