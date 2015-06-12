sealed trait Algebra
case class Add(x: Int, y: Int) extends Algebra

class AlgebraCalcuration {
  def add(x: Int, y: Int): Int =
    x + y
}
