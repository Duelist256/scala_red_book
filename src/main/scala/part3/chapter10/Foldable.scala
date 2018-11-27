package part3.chapter10

import part1.chapter3.{Branch, Leaf, Tree}
import part1.chapter5.Stream

trait Foldable[F[_]] {
  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B
  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B
  def foldMap[A, B](as: F[A])(f: A => B)(m: Monoid[B]): B
  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  /* Exercise 10.15
     Any Foldable structure can be turned into a List. Write this conversion in a generic way:*/
  def toList[A](fa: F[A]): List[A] = foldRight(fa)(List[A]())(_ :: _)
}

object Foldable {
  /* Exercise 10.12
     Implement Foldable[List], Foldable[IndexedSeq], and Foldable[Stream].
     Remember that foldRight, foldLeft, and foldMap can all be implemented in terms
     of each other, but that might not be the most efficient implementation.*/
  val listFoldable = new Foldable[List] {
    override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
      as.foldRight(z)(f)
    override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
      as.foldLeft(z)(f)
    override def foldMap[A, B](as: List[A])(f: A => B)(m: Monoid[B]): B =
      Monoid.foldMap(as, m)(f)
  }

  val indexedSeqFoldable = new Foldable[IndexedSeq] {
    override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B =
      as.foldRight(z)(f)
    override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B =
      as.foldLeft(z)(f)
    override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(m: Monoid[B]): B =
      Monoid.foldMapV(as, m)(f)
  }

  val streamFoldable = new Foldable[Stream] {
    override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B =
      as.foldRight(z)(f)
    override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B =
      as.foldLeft(z)(f)
    override def foldMap[A, B](as: Stream[A])(f: A => B)(m: Monoid[B]): B =
      foldLeft(as)(m.zero)((b, a) => m.op(b, f(a)))
  }

  /* Exercise 10.13
     Recall the binary Tree data type from chapter 3. Implement a Foldable instance for it. */
  val treeFoldable = new Foldable[Tree] {
    override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B =
      as match {
        case Leaf(v) => f(v, z)
        case Branch(left, right) => foldRight(left)(foldRight(right)(z)(f))(f)
      }

    override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B =
      as match {
        case Leaf(v) => f(z, v)
        case Branch(left, right) => foldLeft(right)(foldLeft(left)(z)(f))(f)
      }

    override def foldMap[A, B](as: Tree[A])(f: A => B)(m: Monoid[B]): B =
      Tree.fold(as)(f)(m.op)
  }

  /* Exercise 10.14
     Write a Foldable[Option] instance.*/
  val optionFoldable = new Foldable[Option] {
    override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B =
      as.foldRight(z)(f)
    override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B =
      as.foldLeft(z)(f)
    override def foldMap[A, B](as: Option[A])(f: A => B)(m: Monoid[B]): B =
      foldLeft(as)(m.zero)((b, a) => m.op(b, f(a)))
  }
}