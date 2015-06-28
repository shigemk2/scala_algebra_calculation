case class Hoge(x: List[Int])
val a = Hoge(List(1,2,3))
// これは動く
println(a match { case Hoge(x) => x })

case class Fuga(x: Int*)
val b = Fuga(1,2,3)
// これは動かない
println(b match { case Fuga(x).toList => x })

