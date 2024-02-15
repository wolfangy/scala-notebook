//package chapter2.example

object Program /*extends App*/{
  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  private def formatAbs(x: Int): String = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  private def formatResult(str: String, n: Int, f: Int => Int) = "The %s of %d is %d.".format(str, n, f(n))


  def factorial(n: Int): Int = {
    def go(n: Int, acc: Int): Int =
      if(n <= 0) acc
      else go(n - 1, n  * acc)

    go(n, 1)

  }

  def fib(n: Int): Int = {
    def go(i: Int, x: Int, acc: Int): Int = {
      if (i <= 0) acc
      else go(i - 1, acc, x + acc)
    }

    go(n, 0, 1)
  }

  def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean =
    if (as.length <= 1) true
    else as.tail.zip(as).foldLeft(true)(_ && gt.tupled(_))

  def curry[A, B, C](f: (A, B) => C): A => B =>C =
    a => b => f(a, b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)


  def compose[A, B, C] (f: B => C, g: A => B): A => C =
    a => f(g(a))

  println(formatResult("abs", -42, abs))
  println(formatResult("factorial", 10, factorial))
  println(formatResult("fib", 6, fib))
  println(s"is the list sorted: ${isSorted[Int](Array(1,2,1), (a, b)=> a > b)}")
}