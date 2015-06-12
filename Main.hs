module Main where

import Test.HUnit
import System.IO

data Add = Add Int Int deriving Show

eval (Add x y) = x + y

tests = TestList
    [ "eval 1" ~: eval (Add 1 1) ~?= 1+1
    , "eval 2" ~: eval (Add 2 3) ~?= 2+3
    ]

main = do
    runTestText (putTextToHandle stderr False) tests
