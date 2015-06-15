module Main where

import Test.HUnit
import System.IO

data Expr = N Int
          | Add [Expr]
          | Mul [Expr]
          deriving Show

eval (N   x ) = x
eval (Add xs) = sum     [eval x | x <- xs]
eval (Mul xs) = product [eval x | x <- xs]

tests = TestList
    [ "eval 1" ~: eval (Add[N 1,N  1 ]) ~?= 1+1
    , "eval 2" ~: eval (Add[N 2,N  3 ]) ~?= 2+3
    , "eval 3" ~: eval (Add[N 5,N(-3)]) ~?= 5-3
    , "eval 4" ~: eval (Mul[N 3,N  4 ]) ~?= 3*4
    , "eval 5" ~: eval (Add[N 1,Mul[N 2,N 3]]) ~?= 1+2*3
    , "eval 6" ~: eval (Mul[Add[N 1,N 2],N 3]) ~?= (1+2)*3
    ]

main = do
    runTestText (putTextToHandle stderr False) tests
