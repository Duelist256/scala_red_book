package part3.chapter10

object MonoidInstances {
  val stringMonoid = new Monoid[String] {
    override def op(a1: String, a2: String): String = a1 + a2
    override def zero: String = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    override def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2
    override def zero: List[A] = Nil
  }

  /* Exercise 10.1
     Give Monoid instances for integer addition and multiplication as well as the Boolean
     operators. */
  val intAddition: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2
    override def zero: Int = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 * a2
    override def zero: Int = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    override def zero: Boolean = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    override def zero: Boolean = true
  }

  /* Exercise 10.2
     Give a Monoid instance for combining Option values. */
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1.orElse(a2)
    override def zero: Option[A] = None
  }

  /* Exercise 10.3
     A function having the same argument and return type is sometimes called an endofunction.
     2 Write a monoid for endofunctions. */
  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override def op(a1: A => A, a2: A => A): A => A = a1.compose(a2)
    override def zero: A => A = identity
  }

  /* Exercise 10.10
     Write a monoid instance for WC and make sure that it meets the monoid laws. */
  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    override def op(a1: WC, a2: WC): WC = (a1, a2) match {
      case (Stub(ch), Stub(ch2)) => Stub(ch + ch2)
      case (Stub(ch), Part(lch, words, rch)) => Part(ch + lch, words, rch)
      case (Part(lch, words, rch), Stub(ch2)) => Part(lch, words, rch + ch2)
      case (Part(lch, words, rch), Part(lch2, words2, rch2)) =>
        val n = if ((rch + lch2).nonEmpty) 1 else 0
        Part(lch, words + n, rch2)
    }
    override def zero: WC = Stub("")
  }

  /* TODO Exercise 10.11
     Use the WC monoid to implement a function that counts words in a String by recursively
     splitting it into substrings and counting the words in those substrings. */

  /* TODO Exercise 10.16
     Prove it. Notice that your implementation of op is obviously associative so long as A.op
     and B.op are both associative. */
  def productMonoid[A,B](A: Monoid[A], B: Monoid[B]): Monoid[(A,B)] = ???
}