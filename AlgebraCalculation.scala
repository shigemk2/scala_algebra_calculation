sealed trait Algebra
case class Add(x: Int, y: Int) extends Algebra

def add(a: Add): Int =
  a.x + a.y

println(add(new Add(1,2)) == 3)
println(add(new Add(2,3)) == 5)
