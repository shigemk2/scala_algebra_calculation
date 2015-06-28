sealed trait Expr
case class N(n: Int) extends Expr
case class Add(n: List[Expr]) extends Expr
case class Mul(n: List[Expr]) extends Expr

def str(e: Expr): String = e match {
  case N(x) => x.toString
  case Add(x) if x.isEmpty => ""
  case Add(x) if x.length == 1 => str(x.head)
  case Add(x::y::zs) if isneg(y) => str(x) ++ str(Add(y::zs))
  case Add(x::xs) => str(x) ++ "+" ++ str(Add(xs))
  // case Mul(xs) => xs.map(x => eval(x)).product.toString
}

def isneg(e: Expr): Boolean = e match {
  case N(n) if n < 0 => true
  case _          => false
}

println(str(N(1)) == "1")
println(str(Add(List(N(1),N(2)))) == "1+2")
// println(eval(Add(List(N(2),N(3)))) == 2+3)
// println(eval(Add(List(N(5),N(-3)))) == 5-3)
// println(eval(Mul(List(N(3),N(4)))) == 3*4)
// println(eval(Add(List(N(1),Mul(List(N(2),N(3)))))) == 1+2*3)
// println(eval(Mul(List(Add(List(N(1),N(2))),N(3)))) == (1+2)*3)

