//package chapter3.example

enum List[+A]:
  case Nil
  case Cons(head: A, tail: List[A])

object List{
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons (as.head, List.apply(as.tail*))

  def sum(ints: List[Int]): Int = ints match
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)

  def product(doubles: List[Double]): Double = doubles match
    case Nil => 1.0
    case Cons (0.0, _) => 0.0
    case Cons (x, xs) => x * product(xs)

  def init[A](as: List[A]): List[A] = as match
    case Nil => Nil
    case Cons(head, Cons(tail, Nil)) => Cons(head, Nil)
    case Cons (x, xs) => Cons (x, init(xs))

  def map[A, B](as: List[A])(f: A => B): List[B] = as match
    case Nil => Nil
    case Cons(x, xs) => Cons(f(x), map(xs)(f))

  def foldRight[A, B](as: List[A], acc: B)(f: (A, B) => B): B =
    as match {
      case Nil => acc
      case Cons(x, xs) => f(x, foldRight(xs, acc)(f))
    }

  @annotation.tailrec
  def foldLeft[A, B](as: List[A], acc: B)(f: (A, B) => B): B =
    as match {
      case Nil => acc
      case Cons(x, xs) => foldLeft(xs, f(x, acc))(f)
    }

  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, Nil: List[A])(Cons(_, _))

  def foldRightViaFoldLeft[A, B](as: List[A], acc: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), acc)(f)

  def length[A](as: List[A]): Int =
    foldRight(as, 0)((_, acc) => acc + 1)

  def append[A](as1: List[A], as2: List[A]): List[A] =
    foldRight(as1, as2)(Cons(_, _))

  def dropWhile[A](as: List[A])(f: A => Boolean): List[A] = as match
    case Cons(h, t) if f(h) => dropWhile(t)(f)
    case _ => as

  def concat[A](as: List[List[A]]): List[A] =
    foldRight(as, Nil: List[A])(append)

  def map_[A, B](as: List[A])(f: A => B): List[B] =
    foldRight(as, Nil: List[B])((cur, acc) => Cons(f(cur), acc))

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    concat(map(as)(f))
}
