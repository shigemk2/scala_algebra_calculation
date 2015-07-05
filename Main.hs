module Main where

import Test.HUnit
import System.IO

data Expr = N Int
          | Var String Int Int
          | Add [Expr]
          | Mul [Expr]
          deriving (Show, Eq)

x a n = Var "x" a n

eval (N   x ) = x
eval (Add xs) = sum     [eval x | x <- xs]
eval (Mul xs) = product [eval x | x <- xs]

isneg (N n)       | n < 0 = True
isneg (Var _ a _) | a < 0 = True
isneg _                   = False

str (N x) = show x
str (Var x  1 1) = x
str (Var x(-1)1) = "-" ++ x
str (Var x  a 1) = show a ++ x
str (Var x  a n) = str (Var x a 1) ++ "^" ++ show n
str (Add [])       = ""
str (Add [Add xs]) = "(" ++ str (Add xs) ++ ")"
str (Add [x])      = str x
str (Add (x:y:zs))
    | isneg y      = str (Add [x])        ++ str (Add (y:zs))
str (Add (x:xs))   = str (Add [x]) ++ "+" ++ str (Add xs)
str (Mul [])       = ""
str (Mul [Add xs]) = "(" ++ str (Add xs) ++ ")"
str (Mul [Mul xs]) = "(" ++ str (Mul xs) ++ ")"
str (Mul [x])      = str x
str (Mul (x:xs))   = str (Mul [x]) ++ "*" ++ str (Mul xs)

xlt (Var "x" _ n1) (Var "x" _ n2) = n1 < n2
xlt (Var "x" _ _ ) _              = False
xlt _              (Var "x" _ _ ) = True
xlt _              _              = True -- ignore

xsort (Add xs) = Add $ f xs
    where
        f []     = []
        f (x:xs) = f xs1 ++ [x] ++ f xs2
            where
                xs1 = [xsort x' | x' <- xs, not $ x' `xlt` x]
                xs2 = [xsort x' | x' <- xs,       x' `xlt` x]
xsort (Mul xs) = Mul [xsort x | x <- xs]
xsort x = x

tests = TestList
    [ "eval 1" ~: eval (Add[N 1,N  1 ]) ~?= 1+1
    , "eval 2" ~: eval (Add[N 2,N  3 ]) ~?= 2+3
    , "eval 3" ~: eval (Add[N 5,N(-3)]) ~?= 5-3
    , "eval 4" ~: eval (Mul[N 3,N  4 ]) ~?= 3*4
    , "eval 5" ~: eval (Add[N 1,Mul[N 2,N 3]]) ~?= 1+2*3
    , "eval 6" ~: eval (Mul[Add[N 1,N 2],N 3]) ~?= (1+2)*3
    , "str 1" ~: str (Add[N 1,N  2 ,N  3 ])  ~?= "1+2+3"
    , "str 2" ~: str (Add[N 1,N(-2),N(-3)])  ~?= "1-2-3"
    , "str 3" ~: str (Mul[N 1,N  2 ,N  3 ])  ~?= "1*2*3"
    , "str 4" ~: str (Add[N 1,Mul[N 2,N 3]]) ~?= "1+2*3"
    , "str 5" ~: str (Add[Add[N 1,N 2],N 3]) ~?= "(1+2)+3"
    , "str 6" ~: str (Mul[Add[N 1,N 2],N 3]) ~?= "(1+2)*3"
    , "str 7" ~: str (Mul[Mul[N 1,N 2],N 3]) ~?= "(1*2)*3"
    , "equal" ~: Add[N 1,N 2] ~?= Add[N 1,N 2]
    , "x 1" ~: str (Add [1`x`1,N 1]) ~?= "x+1"
    , "x 2" ~: str (Add [1`x`3,(-1)`x`2,(-2)`x`1,N 1]) ~?= "x^3-x^2-2x+1"
    , "xlt 1" ~: (1`x`1) `xlt` (1`x`2) ~?= True
    , "xlt 2" ~: (N 1)   `xlt` (1`x`1) ~?= True
    , "xsort 1" ~:
        let f = Add[1`x`1,N 1,1`x`2]
        in (str f, str $ xsort f)
        ~?= ("x+1+x^2", "x^2+x+1")
    , "xsort 2" ~:
        let f = Mul[Add[N 5,2`x`1],Add[1`x`1,N 1,1`x`2]]
        in (str f, str $ xsort f)
        ~?= ("(5+2x)*(x+1+x^2)", "(2x+5)*(x^2+x+1)")
    , "xsort 3" ~:
        let f = Mul[Add[N 5,2`x`1],Add[1`x`1,N 1,1`x`2]]
        in (xsort f)
        ~?= Mul[Add[2`x`1,N 5],Add[1`x`2,1`x`1,N 1]]
    ]

main = do
    runTestText (putTextToHandle stderr False) tests
