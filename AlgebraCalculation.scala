sealed trait Expr {
  def eval(): Int = this match {
    case N(x) => x
    case Add(xs) => xs.map(x => x.eval).sum
    case Mul(xs) => xs.map(x => x.eval).product
  }
}
case class N(n: Int) extends Expr
case class Add(n: List[Expr]) extends Expr
case class Mul(n: List[Expr]) extends Expr

println(Add(List(N(1),N(2))).eval == 1+2)
println(Mul(List(N(3),N(4))).eval == 3*4)
println(Mul(List(Add(List(N(1),N(2))),N(3))).eval == (1+2)*3)
// println(eval(Mul(Add(N(1),N(2)),N(3))) == (1+2)*3)
