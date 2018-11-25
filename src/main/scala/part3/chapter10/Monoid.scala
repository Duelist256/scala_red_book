package part3.chapter10

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {
  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  /* Exercise 10.5
     Implement foldMap.*/
  def foldMap[A,B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  /* TODO Exercise 10.6
     Hard: The foldMap function can be implemented using either foldLeft or fold-
     Right. But you can also write foldLeft and foldRight using foldMap! Try it. */

}