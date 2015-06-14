sealed trait Algebra
case class Add(n: Int*) extends Algebra
case class Mul(n: Int*) extends Algebra

def eval(a: Add): Int =
  a.n.sum

def eval(a: Mul): Int =
  a.n.product

println(eval(new Add(1,2)) == 3)
println(eval(new Add(2,3)) == 5)
println(eval(new Add(2,3,3)) == 8)
println(eval(new Add(5,-3)) == 2)
println(eval(new Mul(3,4)) == 12)
