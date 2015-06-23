sealed trait Expr
case class N(n: Int) extends Expr
case class Add(n: N*) extends Expr
case class Mul(n: N*) extends Expr

def eval(x: N): Int =
  x.n

def eval(xs: Add): Int =
  xs.n.map(x => eval(x)).sum

def eval(xs: Mul): Int =
  xs.n.map(x => eval(x)).product

println(eval(Add(N(1),N(2))))
println(eval(Add(N(1),N(2))) == 1+2)
println(eval(Add(N(2),N(3))) == 2+3)
println(eval(Add(N(2),N(3),N(3))) == 2+3+3)
println(eval(Add(N(5),N(-3))) == 5-3)
println(eval(Mul(N(3),N(4))) == 3*4)
println(eval(Mul(Add(N(1),N(2)),N(3))) == (1+2)*3)
