# purescript-lp
Linear programming library in Purescript

### Example

Consider the following problem:

maximize $7x + 9y + 18z + 17t$

subject to

$$\left\\{
\begin{array}{c}
2x + 4y + 5z + 7t \leq 42\\
x + y + 2z + 2t \leq 17\\
x + 2y + 3z + 3t \leq 24\\
x \geq 0, y \geq 0, z \geq 0, t \geq 0
\end{array} 
\right.
$$

It can be solved using the following program

```purescript
module Main where

import Prelude
import Data.Rational ((%))
import Effect (Effect)
import Data.LinearProgramming.Program (Program(..), Objective(..), (++), (**), (&<=&), solveLinearProgram)
import Effect.Console (logShow)

main :: Effect Unit
main = do
  let p = Program 
    { objective: Maximize $ 7 ** "x1" ++ 9 ** "x2" ++ 18 ** "x3" ++ 17 ** "x4"
    , constraints: 
        [ 2 ** "x1" ++ 4 ** "x2" ++ 5 ** "x3" ++ 7 ** "x4" &<=& 42
        , 1 ** "x1" ++ 1 ** "x2" ++ 2 ** "x3" ++ 2 ** "x4" &<=& 17
        , 1 ** "x1" ++ 2 ** "x2" ++ 3 ** "x3" ++ 3 ** "x4" &<=& 24
        ]
    } <#> (_ % 1)
  logShow $ solveLinearProgram p
```

or alternatively

```purescript
module Main where

import Prelude
import Data.Rational ((%))
import Effect (Effect)
import Data.LinearAlgebra.Matrix as M
import Data.LinearAlgebra.Vector as V
import Data.LinearProgramming.Simplex (simplex)
import Effect.Console (logShow)

main :: Effect Unit
main = do
  let a = M.fromArray 3 4 [ [2, 4, 5, 7]
                          , [1, 1, 2, 2]
                          , [1, 2, 3, 3]
                          ] <#> (_ % 1)
  let b = V.fromArray [42, 17, 24] <#> (_ % 1)
  let obj = V.fromArray [7, 9, 18, 17] <#> (_ % 1)
  logShow $ simplex a b obj
```
