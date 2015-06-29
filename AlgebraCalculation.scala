sealed trait Expr
case class N(n: Int) extends Expr
case class Add(n: Expr*) extends Expr
case class Mul(n: Expr*) extends Expr

def str(e: Expr): String = e match {
  case N(x)             => x.toString
  case Add()            => ""
  case Add(x)           => str(x)
  case Add(x, xs@_*)
      if isneg(xs.head) => str(x) ++ str(Add(xs: _*))
  case Add(x, xs@_*)    => str(x) ++ "+" ++ str(Add(xs: _*))
  case Mul()            => ""
  case Mul(x)           => str(x)
  case Mul(x, xs@_*)    => str(x) ++ "*" ++ str(Mul(xs: _*))
}

def isneg(e: Expr): Boolean = e match {
  case N(n) if n < 0 => true
  case _          => false
}

println(str(Add(N(1),N(2),N(3))) == "1+2+3")
println(str(Add(N(1),N(-2),N(-3))) == "1-2-3")
println(str(Mul(N(1),N(2),N(3))) == "1*2*3")
println(str(Add(N(1),Mul(N(2),N(3)))) == "1+2*3")
