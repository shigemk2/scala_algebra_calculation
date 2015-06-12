sealed trait Algebra
case class Add(x: Int, y: Int) extends Algebra

def add(x: Int, y: Int): Int =
  x + y

println(add(1,1) == 2)
println(add(2,3) == 5)
