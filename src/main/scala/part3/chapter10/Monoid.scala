package part3.chapter10

import part2.chapter7.Par

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

  /* TODO Exercise 10.7
     Implement a foldMap for IndexedSeq.4 Your implementation should use the strategy
     of splitting the sequence in two, recursively processing each half, and then adding the
     answers together with the monoid.*/
  def foldMapV[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = ???

  /* TODO Exercise 10.8
     Hard: Also implement a parallel version of foldMap using the library we developed in
     chapter 7. Hint: Implement par, a combinator to promote Monoid[A] to a Monoid
     [Par[A]],5 and then use this to implement parFoldMap.*/
  def par[A](m: Monoid[A]): Monoid[Par[A]] = ???
  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = ???
}