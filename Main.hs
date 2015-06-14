module Main where

import Test.HUnit
import System.IO

data Expr = Add [Int]
          | Mul [Int]
          deriving Show

eval (Add xs) = sum xs
eval (Mul xs) = product xs

tests = TestList
    [ "eval 1" ~: eval (Add [1, 1]) ~?= 1+1
    , "eval 2" ~: eval (Add [2, 3]) ~?= 2+3
    , "eval 3" ~: eval (Add [5, -3]) ~?= 5-3
    , "eval 4" ~: eval (Mul [3, 4]) ~?= 3*4
    ]

main = do
    runTestText (putTextToHandle stderr False) tests
