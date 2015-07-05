sealed trait Expr
case class N(n: Int) extends Expr
case class Var(x: String, n: Int) extends Expr
case class Add(n: Expr*) extends Expr
case class Mul(n: Expr*) extends Expr

def x(n: Int): Var = {
  Var("x", n)
}

def eval(e: Expr): Int = (e: @unchecked) match {
  case N(x) => x
  case Add(xs @ _*) => xs.map(x => eval(x)).sum
  case Mul(xs @ _*) => xs.map(x => eval(x)).product
}

def str(e: Expr): String = e match {
  case N(x)             => x.toString
  case Var(x, 1)        => x
  case Var(x, n)        => x ++ "^" ++ n.toString
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
  case _          => false
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
println(str(Add(x(1),N(1))) == "x+1")
println(str(Add(x(2),x(1),N(1))) == "x^2+x+1")
