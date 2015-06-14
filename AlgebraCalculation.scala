sealed trait Algebra
case class Add(n: Int*) extends Algebra

def add(n: Int*): Int =
  n.sum

println(add(1,1) == 2)
println(add(2,3) == 5)
println(add(2,3,3) == 8)
println(add(2,3,3) == 7)
println(add(5,-3) == 2)
