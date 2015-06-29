sealed trait Expr
case class N(n: Int) extends Expr
case class Add(n: Expr*) extends Expr
case class Mul(n: Expr*) extends Expr

def eval(e: Expr): Int = e match {
  case N(x) => x
  case Add(xs @ _*) => xs.map(x => eval(x)).sum
  case Mul(xs @ _*) => xs.map(x => eval(x)).product
}

println(eval(Add(N(1),N(2))) == 1+2)
println(eval(Add(N(2),N(3))) == 2+3)
println(eval(Add(N(5),N(-3))) == 5-3)
println(eval(Mul(N(3),N(4))) == 3*4)
println(eval(Add(N(1),Mul(N(2),N(3)))) == 1+2*3)
println(eval(Mul(Add(N(1),N(2)),N(3))) == (1+2)*3)

