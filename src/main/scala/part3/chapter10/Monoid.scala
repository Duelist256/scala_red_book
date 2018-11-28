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

  /* Exercise 10.6
     Hard: The foldMap function can be implemented using either foldLeft or fold-
     Right. But you can also write foldLeft and foldRight using foldMap! Try it. */
  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = {
    val m = new Monoid[B => B] {
      override def op(a1: B => B, a2: B => B): B => B = a2 compose a1
      override def zero: B => B = identity
    }
    foldMap(as, m)(a => b => f(b, a))(z)
  }

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, MonoidInstances.endoMonoid[B])(a => b => f(a, b))(z)

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
  /* Solved for ascending order */
  def isOrdered(is: IndexedSeq[Int]): Boolean = {
    val m = new Monoid[(Int, Boolean)] {
      override def op(a1: (Int, Boolean), a2: (Int, Boolean)): (Int, Boolean) =
        (a1, a2) match {
          case ((_, b1), (i2, b2)) if !(b1 && b2) => (i2, false)
          case ((i1, _), (i2, _)) if i1 <= i2 => (i2, true)
          case ((_, _), (i2, _)) => (i2, false)
        }
      override def zero: (Int, Boolean) = (Int.MinValue, true)
    }
    foldMapV(is, m)(i => (i, true))._2
  }

  def mapMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K, V]] =
    new Monoid[Map[K, V]] {
      def zero: Map[K, V] = Map[K,V]()
      def op(a: Map[K, V], b: Map[K, V]): Map[K, V] =
        (a.keySet ++ b.keySet).foldLeft(zero) { (acc,k) =>
          acc.updated(k, V.op(a.getOrElse(k, V.zero),
            b.getOrElse(k, V.zero)))
        }
    }

  /* Exercise 10.17
     Write a monoid instance for functions whose results are monoids. */
  def functionMonoid[A,B](B: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    override def op(a1: A => B, a2: A => B): A => B =
      a => B.op(a1(a), a2(a))
    override def zero: A => B = _ => B.zero
  }

  /* Exercise 10.18
     A bag is like a set, except that it’s represented by a map that contains one entry per
     element with that element as the key, and the value under that key is the number of
     times the element appears in the bag. For example:
     scala> bag(Vector("a", "rose", "is", "a", "rose"))
     res0: Map[String,Int] = Map(a -> 2, rose -> 2, is -> 1)
     Use monoids to compute a “bag” from an IndexedSeq. */
  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    foldMapV(as, mapMergeMonoid[A, Int](MonoidInstances.intAddition))(a => Map[A, Int](a -> 1))
}