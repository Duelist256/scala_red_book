package chapter2.part3

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  /* Exercise 3.2 */
  def tail[A](xs: List[A]): List[A] = {
    xs match {
      case Cons(y, ys) => ys
      case Nil => Nil
    }
  }

  /* Exercise 3.3 */
  def setHead[A](elem: A, xs: List[A]): List[A] = {
    xs match {
      case Cons(y, ys) => Cons(elem, ys)
      case Nil => Cons(elem, Nil)
    }
  }

  /* Exercise 3.4 */
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n == 0 || l == Nil) l
    else drop(tail(l), n - 1)
  }

  /* Exercise 3.5 */
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Cons(x, xs) if f(x) => dropWhile(xs, f)
      case _ => l
    }
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  /* Exercise 3.6 */
  def init[A](l: List[A]): List[A] = {
    def loop(l: List[A], acc: List[A]): List[A] = {
      l match {
        case Cons(x, Nil) => acc
        case Cons(x, xs) => loop(xs, append(acc, List(x)))
        case Nil => acc
      }
    }
    loop(l, Nil)
  }
}