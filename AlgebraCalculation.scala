sealed trait Algebra[+A]
case class N[A](n: A) extends Algebra[A]
case class Add[A](n: A*) extends Algebra[A]
case class Mul[A](n: A*) extends Algebra[A]

def eval(x: N[Int]): Int =
  x.n

def eval(xs: Add[N[Int]]): Int =
  xs.n.map(x => eval(x)).sum

def eval(xs: Mul[N[Int]]): Int =
  xs.n.map(x => eval(x)).product

println(eval(Add(N(1),N(2))))
println(eval(Add(N(1),N(2))) == 1+2)
println(eval(Add(N(2),N(3))) == 2+3)
println(eval(Add(N(2),N(3),N(3))) == 2+3+3)
println(eval(Add(N(5),N(-3))) == 5-3)
println(eval(Mul(N(3),N(4))) == 3*4)
// println(eval(Mul(Add(N(1),N(2)),N(3))) == (1+2)*3)
