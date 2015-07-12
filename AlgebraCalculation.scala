sealed trait Expr
case class N(n: Int) extends Expr
case class Rational(n: Int, d: Int) extends Expr {
  private def gcd(x: Int, y: Int): Int = {
    if (x == 0) y
    else if (x < 0) gcd(-x, y)
    else if (y < 0) -gcd(x, -y)
    else gcd(y % x, x)
  }
  private val g = gcd(n, d)
  val numer: Int = n/g
  val denom: Int = d/g
  def +(that: Rational) =
    new Rational(numer * that.denom + that.numer * denom,
    denom * that.denom)
  def -(that: Rational) =
  new Rational(numer * that.denom - that.numer * denom,
    denom * that.denom)
  def *(that: Rational) =
    new Rational(numer * that.numer, denom * that.denom)
  def /(that: Rational) =
    new Rational(numer * that.denom, denom * that.numer)
  override def toString(): String = s"${n} % ${d}"
}
case class Var(x: String, a: Int, n: Int) extends Expr
case class Add(n: Expr*) extends Expr
case class Mul(n: Expr*) extends Expr

def x(a: Int, n: Int): Var = {
  Var("x", a, n)
}

def eval(e: Expr): Int = (e: @unchecked) match {
  case N(x) => x
  case Add(xs@_*) => xs.map(eval).sum
  case Mul(xs@_*) => xs.map(eval).product
}

def str(e: Expr): String = e match {
  case N(x)             => x.toString
  case Var(x, 1, 1)     => x
  case Var(x, -1, 1)    => "-" ++ x
  case Var(x, a, 1)     => a.toString ++ x
  case Var(x, a, n)     => str(Var(x, a, 1)) ++ "^" ++ n.toString
  case Add()            => ""
  case Add(Add(xs@_*))  => "(" ++ str(Add(xs: _*)) ++ ")"
  case Add(x)           => str(x)
  case Add(x, xs@_*)
      if isneg(xs.head) => str(Add(x)) ++ str(Add(xs: _*))
  case Add(x, xs@_*)    => str(Add(x)) ++ "+" ++ str(Add(xs: _*))
  case Mul()            => ""
  case Mul(Add(xs@_*))  => "(" ++ str(Add(xs: _*)) ++ ")"
  case Mul(Mul(xs@_*))  => "(" ++ str(Mul(xs: _*)) ++ ")"
  case Mul(x)           => str(x)
  case Mul(x, xs@_*)    => str(Mul(x)) ++ "*" ++ str(Mul(xs: _*))
}

def isneg(e: Expr): Boolean = e match {
  case N(n) if n < 0 => true
  case Var(_, a, _) if a < 0 => true
  case _ => false
}

def xlt(x: Expr, y: Expr): Boolean = (x, y) match {
  case (Var("x", _, n1), Var("x", _, n2)) => n1 < n2
  case (Var("x", _, _), _)                => false
  case (_, Var("x", _, _))                => true
  case _                                  => true
}

def xsort(e: Expr): Expr = e match {
  case Add(xs@_*) => {
    def f(list: List[Expr]): List[Expr] = list match {
      case List() => List()
      case x::xs => {
        val xs1 = for (x1 <- xs if !xlt(x1, x)) yield xsort(x1)
        val xs2 = for (x2 <- xs if  xlt(x2, x)) yield xsort(x2)
        f(xs1) ++ List(x) ++ f(xs2)
      }
    }
    Add(f(xs.toList): _*)
  }
  case Mul(xs@_*) => Mul(xs.map(xsort): _*)
  case _ => e
}

def flatten(list: List[Expr]): List[Expr] = list match {
  case List() => List()
  case Add(xs1@_*)::xs2 => flatten(xs1.toList ++ xs2)
  case x::xs => x :: flatten(xs)
}

def add(list: List[Expr]): Expr = list match {
  case List() => N(0)
  case List(x) => x
  case _ => Add(list: _*)
}

def xsimplify(e: Expr): Expr = e match {
  case Add(xs@_*) => {
    def getxs(e: Expr) = (e: @unchecked) match {
      case Add(xs@_*) => xs
    }
    def f(list: List[Expr]): List[Expr] = list match {
      case List() => List()
      case N(0)::xs => f(xs)
      case Var(_,0,_)::xs => f(xs)
      case List(x) => List(xsimplify(x))
      case N(a1)::N(a2)::zs => f(N(a1 + a2)::zs)
      case Var("x",a1,n1)::Var("x",a2,n2)::zs if n1 == n2 => f(x(a1 + a2, n1)::zs)
      case x::xs => xsimplify(x)::f(xs)
    }
    add(f(getxs(xsort(Add(flatten(xs.toList): _*))).toList))
  }
  case Mul(xs@_*) => Mul(xs.map(xsimplify(_)): _*)
  case _ => e
}

def multiply(e1: Expr, e2: Expr): Expr = (e1, e2) match {
  case (N(n1), N(n2)) => N(n1 * n2)
  case (N(n1), Var(x, a2, n2)) => Var(x, (n1 * a2), n2)
  case (Var(x, a1, n1), N(n2)) => Var(x, (a1 * n2), n1)
  case (Var(x, a1, n1), Var(y, a2, n2)) if x == y => Var(x, (a1 * a2), (n1 + n2))
  case (Var(x, a1, n1), Var(y, a2, n2)) if x != y => Mul(Var(x, a1, n1), Var(y, a2, n2))
  case (Add(xs1@_*), Add(xs2@_*)) => Add((for(x1 <- xs1; x2 <- xs2) yield multiply(x1, x2)): _*)
  case (Add(xs1@_*), x2) => Add((for(x1 <- xs1) yield multiply(x1, x2)): _*)
  case (x1, Add(xs2@_*)) => Add((for(x2 <- xs2) yield multiply(x1, x2)): _*)
  case (Mul(xs1@_*), Mul(xs2@_*)) => Mul(xs1.toList ++ xs2.toList: _*)
  case (Mul(xs1@_*), xs2) => Mul(xs1.toList :+ xs2: _*)
  case (xs1, Mul(xs2@_*)) => Mul(xs1 :: xs2.toList: _*)
}

def mul(list: List[Expr]): Expr = list match {
  case List() => N(1)
  case List(x) => x
  case _ => Mul(list: _*)
}

def expand(e: Expr): Expr = e match {
  case Mul(xs@_*) => {
    def f(list: List[Expr]): List[Expr] = list match {
      case List() => List()
      case List(x) => List(expand(x))
      case (x::y::xs) => multiply(x,y) :: xs
    }
    mul(f(xs.toList))
  }
  case Add(xs@_*) => {
    def f(list: List[Expr]): List[Expr] = list match {
      case List() => List()
      case x::xs => {
        val x2 = expand(x)
        if (x != x2) x2::xs else x::f(xs)
      }
    }
    add(f(xs.toList))
  }
  case _ => e
}

def expandAll(x: Expr): Expr = {
  val x2 = expand(x)
  if (x != x2) expandAll(x2) else x
}

def differentiate(str: String, e: Expr): Expr = (str, e) match {
  case (x, Add(ys@_*)) => Add((for(y <- ys) yield differentiate(x, y)): _*)
  case (x, Var(y, a, 1)) if x == y => N(a)
  case (x, Var(y, a, n)) if x == y => Var(x, a * n,n - 1)
  case (_, Var(_, _, _)) => N(0)
  case (_, N(_)) => N(0)
}

def test[A](tag: String, t: (A, A)) = {
  if (t._1 == t._2) {
    println("[OK] " + tag)
  } else {
    println("[NG] " + tag)
    println("  value   : " + t._1)
    println("  expected: " + t._2)
  }
}

test("eval 1", (eval(Add(N(1),N( 1))), 1+1))
test("eval 2", (eval(Add(N(2),N( 3))), 2+3))
test("eval 3", (eval(Add(N(5),N(-3))), 5-3))
test("eval 4", (eval(Mul(N(3),N( 4))), 3*4))
test("eval 5", (eval(Add(N(1),Mul(N(2),N(3)))), 1+2*3))
test("eval 6", (eval(Mul(Add(N(1),N(2)),N(3))), (1+2)*3))
test("str 1", (str(Add(N(1),N( 2),N( 3))), "1+2+3"))
test("str 2", (str(Add(N(1),N(-2),N(-3))), "1-2-3"))
test("str 3", (str(Mul(N(1),N( 2),N( 3))), "1*2*3"))
test("str 4", (str(Add(N(1),Mul(N(2),N(3)))), "1+2*3"))
test("str 5", (str(Add(Add(N(1),N(2)),N(3))), "(1+2)+3"))
test("str 6", (str(Mul(Add(N(1),N(2)),N(3))), "(1+2)*3"))
test("str 7", (str(Mul(Mul(N(1),N(2)),N(3))), "(1*2)*3"))
test("equal", (Add(N(1),N(2)), Add(N(1),N(2))))
test("x 1", (str(Add(x(1,1),N(1))), "x+1"))
test("x 2", (str(Add(x(1,3),x(-1,2),x(-2,1),N(1))), "x^3-x^2-2x+1"))
test("xlt 1", (xlt(x(1,1),x(1,2)), true))
test("xlt 2", (xlt(N(1),x(1,1)), true))
test("xsort 1", {
  val f = Add(x(1,1),N(1),x(1,2))
  ((str(f), str(xsort(f))),
   ("x+1+x^2", "x^2+x+1")) })
test("xsort 2", {
  val f = Mul(Add(N(5),x(2,1)),Add(x(1,1),N(1),x(1,2)))
  ((str(f), str(xsort(f))),
   ("(5+2x)*(x+1+x^2)", "(2x+5)*(x^2+x+1)")) })
test("xsimplify 1", {
  val f = Add(x(2,1),N(3),x(4,2),x(1,1),N(1),x(1,2))
  ((str(f), str(xsimplify(f))),
   ("2x+3+4x^2+x+1+x^2", "5x^2+3x+4")) })
test("xsimplify 2", {
  val f = Mul(Add(x(1,1),N(0),x(2,1)),Add(x(1,2),Add(N(1),x(2,2)),N(2)))
  ((str(f), str (xsimplify(f))),
   ("(x+0+2x)*(x^2+(1+2x^2)+2)", "3x*(3x^2+3)")) })
test("xsimplify 3", {
  val f = Add(x(1,1),N(1),x(0,2),x(1,1),N(1),x(-2,1),N(-2))
  ((str(f), str(xsimplify(f))),
   ("x+1+0x^2+x+1-2x-2", "0")) })
test("multiply 1", {
  val f1 = N(2)
  val f2 = N(3)
  ((str(f1), str(f2), str(multiply(f1,f2))),
   ("2", "3", "6")) })
test("multiply 2", {
  val f1 = N(2)
  val f2 = x(3,2)
  ((str(f1), str(f2), str(multiply(f1,f2))),
   ("2", "3x^2", "6x^2")) })
test("multiply 3", {
  val f1 = x(2,3)
  val f2 = x(3,4)
  ((str(f1), str(f2), str(multiply(f1,f2))),
   ("2x^3", "3x^4", "6x^7")) })
test("multiply 4", {
  val f1 = N(2)
  val f2 = Add(x(1,1),x(2,2),N(3))
  ((str(f1), str(f2), str(multiply(f1,f2))),
   ("2", "x+2x^2+3", "2x+4x^2+6")) })
test("multiply 5", {
  val f1 = Add(x(1,1),N(1))
  val f2 = Add(x(2,1),N(3))
  val f3 = multiply(f1,f2)
  val f4 = xsimplify(f3)
  ((str(f1), str(f2), str(f3), str(f4)),
   ("x+1", "2x+3", "2x^2+3x+2x+3", "2x^2+5x+3")) })
test("expand 1", {
  val f = Mul(Add(x(1,1),N(1)),Add(x(1,1),N(2)),Add(x(1,1),N(3)))
  ((str(f), str(expand(f))),
   ("(x+1)*(x+2)*(x+3)", "(x^2+2x+x+2)*(x+3)")) })
test("expand 2", {
  val f = Add(N(1),Mul(Add(x(1,1),N(1)),Add(x(1,1),N(2)),Add(x(1,1),N(3))))
  ((str(f), str(expand(f))),
   ("1+(x+1)*(x+2)*(x+3)", "1+(x^2+2x+x+2)*(x+3)")) })
test("expandAll", {
  val f1 = Add(N(1),Mul(Add(x(1,1),N(1)),Add(x(1,1),N(2)),Add(x(1,1),N(3))))
  val f2 = expandAll(f1)
  val f3 = xsimplify(f2)
  ((str(f1), str(f2), str(f3)),
   ("1+(x+1)*(x+2)*(x+3)",
    "1+(x^3+3x^2+2x^2+6x+x^2+3x+2x+6)",
    "x^3+6x^2+11x+7")) })
test("differentiate", {
  val f = Add(x(1,3),x(1,2),x(1,1),N(1))
  ((str(f), str(differentiate("x",f))),
    ("x^3+x^2+x+1",
      "3x^2+2x+1+0")) })
