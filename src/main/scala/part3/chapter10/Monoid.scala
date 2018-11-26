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

  /* Exercise 10.7
     Implement a foldMap for IndexedSeq.4 Your implementation should use the strategy
     of splitting the sequence in two, recursively processing each half, and then adding the
     answers together with the monoid.*/
  def foldMapV[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    if (v.size <= 0) m.zero
    else if (v.size == 1) f(v(0))
    else {
      val (first, second) = v.splitAt(v.size / 2)
      m.op(foldMapV(first, m)(f), foldMapV(second, m)(f))
    }


  /* Exercise 10.8
     Hard: Also implement a parallel version of foldMap using the library we developed in
     chapter 7. Hint: Implement par, a combinator to promote Monoid[A] to a Monoid
     [Par[A]], and then use this to implement parFoldMap.*/
  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    override def op(a1: Par[A], a2: Par[A]): Par[A] = Par.map2(a1, a2)(m.op)
    override def zero: Par[A] = Par.unit(m.zero)
  }

  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    foldMapV(v, par(m))(a => Par.lazyUnit(f(a)))

  /* Exercise 10.9
     Hard: Use foldMap to detect whether a given IndexedSeq[Int] is ordered. You’ll need
     to come up with a creative Monoid. */
}