sealed trait Algebra
case class Add(n: Int*) extends Algebra

def add(a: Add): Int =
  a.n.sum

println(add(new Add(1,2)) == 3)
println(add(new Add(2,3)) == 5)
