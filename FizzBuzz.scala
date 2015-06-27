trait Nat
trait Zero extends Nat
trait Succ[N <: Nat] extends Nat

type _0 = Zero
type _1 = Succ[_0]
type _2 = Succ[_1]
type _3 = Succ[_2]
type _4 = Succ[_3]
type _5 = Succ[_4]
type _6 = Succ[_5]
type _7 = Succ[_6]
type _8 = Succ[_7]
type _9 = Succ[_8]
type _10 = Succ[_9]
type _11 = Succ[_10]
type _12 = Succ[_11]
type _13 = Succ[_12]
type _14 = Succ[_13]
type _15 = Succ[_14]

trait ToInt[N <: Nat] { def apply(): Int }
object ToInt {
  def apply[N <: Nat](implicit toInt: ToInt[N]) = toInt()
}

trait Mod[N <: Nat, M <: Nat] {
  type Result <: Nat
}

object Mod {
  type Type[N <: Nat, M <: Nat, L <: Nat] = Mod[N, M] { type Result = L }
}

trait Aux[N <: Nat, M <: Nat, L <: Nat, K <: Nat] {
  type Result <: Nat
}

object Aux {
  implicit def zero[M <: Nat, L <: Nat, K <: Nat] = new Aux[Zero, M, L, K] {
    type Result = K
  }
  implicit def succ[N <: Nat, M <: Nat, L <: Nat, K <: Nat](implicit aux: Aux[N, M, L, Succ[K]]) = new Aux[Succ[N], Succ[M], L, K] {
    type Result = aux.Result
  }
  implicit def reset[N <: Nat, L <: Nat, K <: Nat](implicit aux: Aux[N, L, L, Zero]) = new Aux[Succ[N], Zero, L, K] {
    type Result = aux.Result
  }
}

trait FizzBuzz[N <: Nat] {
  type Result
  def apply(): String
}

implicit def zero[N <: Nat] = new Mod[N, Zero] {
  type Result = Zero
}
implicit def succ[N <: Nat, M <: Nat](implicit aux: Aux[N, M, M, Zero]) = new Mod[N, Succ[M]] {
  type Result = aux.Result
}

trait Fizz
trait Buzz

implicit def fizzbuzz[N <: Nat](implicit mod: Mod.Type[N, _15, _0]) = new FizzBuzz[N] {
  type Result = (Fizz, Buzz)
  def apply() = "FizzBuzz"
}

trait FizzBuzzLowestPriorityImplicit {
  implicit def number[N <: Nat](implicit toInt: ToInt[N]) = new FizzBuzz[N] {
    type Result = N
    def apply() = toInt().toString
  }
}

trait FizzBuzzLowPriorityImplicits extends FizzBuzzLowestPriorityImplicit {
  implicit def fizz[N <: Nat](implicit mod: Mod.Type[N, _3, _0]) = new FizzBuzz[N] {
    type Result = Fizz
    def apply() = "Fizz"
  }
  implicit def buzz[N <: Nat](implicit mod: Mod.Type[N, _5, _0]) = new FizzBuzz[N] {
    type Result = Buzz
    def apply() = "Buzz"
  }
}

object FizzBuzz extends FizzBuzzLowPriorityImplicits {
  implicit def fizzbuzz[N <: Nat](implicit mod: Mod.Type[N, _15, _0]) = new FizzBuzz[N] {
    type Result = (Fizz, Buzz)
    def apply() = "FizzBuzz"
  }
}

