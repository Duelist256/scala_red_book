package part1.chapter5

sealed trait Stream[+A] {
  /* TODO Exercise 5.1
     Write a function to convert a Stream to a List, which will force its evaluation and let
     you look at it in the REPL. You can convert to the regular List type in the standard
     library. You can place this and other functions that operate on a Stream inside the
     Stream trait. */
  def toList: List[A] = ???

  /* TODO Exercise 5.2
     Write the function take(n) for returning the first n elements of a Stream, and
     drop(n) for skipping the first n elements of a Stream. */

  /* TODO Exercise 5.3
     Write the function takeWhile for returning all starting elements of a Stream that
     match the given predicate. */
  def takeWhile(p: A => Boolean): Stream[A] = ???
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }
  def empty[A]: Stream[A] = Empty
  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}