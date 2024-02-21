//package chapter3.example

//import chapter3.example.{List, Cons, Nil}
import List.*

object Program{
  @main def run(): Unit =
    val result1 = List(1, 2, 3, 4, 5) match
      case Nil => 42
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 404

    println(s"result is: $result1")

    val result2 = init(List(1, 2, 3, 4, 5))
    println(s"init of List(1, 2, 3, 4, 5) with `map`-> ${map(result2)(_.toString)}")
    println(s"init of List(1, 2, 3, 4, 5) with `foldRight`-> ${foldRight(result2, "")((cur, acc) => acc + cur.toString + ", ")}")
    println(s"init of List(1, 2, 3, 4, 5) with `foldLeft`-> ${foldLeft(result2, "")((cur, acc) => acc + cur.toString + ", ")}")
}