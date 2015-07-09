sealed trait Expr
case class N(n: Int) extends Expr
case class Var(x: String, a: Int, n: Int) extends Expr
case class Add(n: Expr*) extends Expr
case class Mul(n: Expr*) extends Expr

def x(a: Int, n: Int): Var = {
  Var("x", a, n)
}

def eval(e: Expr): Int = (e: @unchecked) match {
  case N(x) => x
  case Add(xs @_*) => xs.map(x => eval(x)).sum
  case Mul(xs @_*) => xs.map(x => eval(x)).product
}

def str(e: Expr): String = e match {
  case N(x)             => x.toString
  case Var(x, 1, 1)     => x
  case Var(x, -1, 1)    => "-" ++ x
  case Var(x, a, 1)     => a.toString ++ x
  case Var(x, a, n)     => str(Var(x, a, 1)) ++ "^" ++ n.toString
  case Add()            => ""
  case Add(Add(xs@_*))  => "(" ++ str(Add(xs: _*)) ++ ")"
  case Add(x)           => str(x)
  case Add(x, xs@_*)
      if isneg(xs.head) => str(Add(x)) ++ str(Add(xs: _*))
  case Add(x, xs@_*)    => str(Add(x)) ++ "+" ++ str(Add(xs: _*))
  case Mul()            => ""
  case Mul(Add(xs@_*))  => "(" ++ str(Add(xs: _*)) ++ ")"
  case Mul(Mul(xs@_*))  => "(" ++ str(Mul(xs: _*)) ++ ")"
  case Mul(x)           => str(x)
  case Mul(x, xs@_*)    => str(Mul(x)) ++ "*" ++ str(Mul(xs: _*))
}

def isneg(e: Expr): Boolean = e match {
  case N(n) if n < 0 => true
  case Var(_, a, _) if a < 0 => true
  case _          => false
}

def xlt(x: Expr, y: Expr): Boolean = (x,y) match {
  case (Var("x", _, n1), Var("x", _, n2)) => (n1 < n2)
  case (Var("x", _, _), _)                => false
  case (_, Var("x", _, _))                => true
  case (_, _)                             => true
}

def xsort(xs: Expr): Expr = xs match {
  case Add(xs@_*)  => {
    def f(xs: List[Expr]): List[Expr] = xs match {
      case List() => List()
      case (x::xs) => {
        val xs1 = for (x1 <- xs if ! xlt(x1, x)) yield xsort(x1)
        val xs2 = for (x2 <- xs if xlt(x2, x)) yield xsort(x2)
        f(xs1) ++ List(x) ++ f(xs2)
      }
    }
    Add(f(xs.toList): _*)
  }
  case Mul(xs@_*)  => Mul(xs.map(x => xsort(x)): _*)
  case xs  => xs
}

def flatten(xs: List[Expr]): List[Expr] = xs match {
  case List() => List()
  case (xs1::xs2) => flatten(xs1 :: xs2)
  case (x::xs) => x :: flatten(xs)
}

def add(xs: List[Expr]): Expr = xs match {
  case List() => N(0)
  case List(xs) => xs
  case xs => Add(xs: _*)
}

def xsimplify(xs: Expr): Expr = xs match {
  case Add(xs@_*)  => {
    def getxs(xs: Add) = xs
    def f(xs: List[Expr]): List[Expr] = xs match {
      case List() => List()
      case (N(0)::xs) => f(xs)
      case (Var(_,0,_)::xs) => f(xs)
      case List(x) => List(xsimplify(x))
      case (N(a1)::N(a2)::zs) => f(N(a1 + a2)::zs)
      case (Var("x",a1,n1)::Var("x",a2,n2)::zs) if n1 == n2 => f(x(a1 + a2, n1)::zs)
      case (x::xs) => xsimplify(x)::f(xs)
    }
    Add(f(xs.toList): _*)
  }
  case Mul(xs@_*)  => Mul(xs.map(x => xsimplify(x)): _*)
  case xs  => xs
}

println(eval(Add(N(1),N(2))) == 1+2)
println(eval(Add(N(2),N(3))) == 2+3)
println(eval(Add(N(5),N(-3))) == 5-3)
println(eval(Mul(N(3),N(4))) == 3*4)
println(eval(Add(N(1),Mul(N(2),N(3)))) == 1+2*3)
println(eval(Mul(Add(N(1),N(2)),N(3))) == (1+2)*3)
println(str(Add(N(1),N(2),N(3))) == "1+2+3")
println(str(Add(N(1),N(-2),N(-3))) == "1-2-3")
println(str(Mul(N(1),N(2),N(3))) == "1*2*3")
println(str(Add(N(1),Mul(N(2),N(3)))) == "1+2*3")
println(str(Mul(N(1),N(2),N(3))) == "1*2*3")
println(str(Add(Add(N(1),N(2)),N(3))) == "(1+2)+3")
println(str(Mul(Add(N(1),N(2)),N(3))) == "(1+2)*3")
println(str(Mul(Mul(N(1),N(2)),N(3))) == "(1*2)*3")
println(Add(N(1),N(2)) == Add(N(1),N(2)))
println(str(Add(x(1,1),N(1))) == "x+1")
println(str(Add(x(1,3),x(-1,2),x(-2,1),N(1))) == "x^3-x^2-2x+1")
val f = Mul(Add(N(5),x(2,1)),Add(x(1,2),x(1,1),N(1),x(3,3)))
println(str(f) == "(5+2x)*(x^2+x+1+3x^3)")
println(str(xsort(f)) == "(2x+5)*(3x^3+x^2+x+1)")
