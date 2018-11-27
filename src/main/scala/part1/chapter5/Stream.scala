package part1.chapter5

sealed trait Stream[+A] {
  /* Exercise 5.1
     Write a function to convert a Stream to a List, which will force its evaluation and let
     you look at it in the REPL. You can convert to the regular List type in the standard
     library. You can place this and other functions that operate on a Stream inside the
     Stream trait. */
  def toList: List[A] = this match {
    case Empty => List.empty
    case Cons(h, t) => h() :: t().toList
  }


  /* Exercise 5.2
     Write the function take(n) for returning the first n elements of a Stream, and
     drop(n) for skipping the first n elements of a Stream. */
  def take(n: Int): Stream[A] =
    if (n <= 0) Empty
    else this match {
      case Empty => Empty
      case Cons(h, t) => Stream.cons(h(), t().take(n - 1))
    }

  /* Exercise 5.3
     Write the function takeWhile for returning all starting elements of a Stream that
     match the given predicate. */
  def takeWhile(p: A => Boolean): Stream[A] =
    this match {
      case Empty => Empty
      case Cons(h, t) =>
        val head = h()
        if (p(head)) Stream.cons(head, t().takeWhile(p))
        else Empty
    }

  def foldLeft[B](z: => B)(f: (=> B, A) => B): B =
    this match {
      case Cons(h,t) => f(t().foldLeft(z)(f), h())
      case _ => z
    }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

  /* Exercise 5.4
     Implement forAll, which checks that all elements in the Stream match a given predicate.
     Your implementation should terminate the traversal as soon as it encounters a
     nonmatching value. */
  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  /* Exercise 5.5
     Use foldRight to implement takeWhile. */
  def takeWhileFR(p: A => Boolean): Stream[A] = foldRight[Stream[A]](Empty) {
    case (a, b) => if (p(a)) Stream.cons(a, b) else b
  }

  /* Exercise 5.6
     Hard: Implement headOption using foldRight. */
  def headOption(): Option[A] = foldRight[Option[A]](None)((h, _) => Some(h))

  /* Exercise 5.7
     Implement map, filter, append, and flatMap using foldRight. The append method
     should be non-strict in its argument. */
  def map[B](f: A => B): Stream[B] = foldRight[Stream[B]](Empty)((h, acc) => Stream.cons(f(h), acc))
  def append[B >: A](other: => Stream[B]): Stream[B] = foldRight[Stream[B]](other)((h, acc) => Stream.cons(h, acc))
  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight[Stream[B]](Empty)((a, acc) => f(a).append(acc))

  /* Exercise 5.13
     Use unfold to implement map, take, takeWhile, zipWith (as in chapter 3), and
     zipAll. The zipAll function should continue the traversal as long as either stream
     has more elements—it uses Option to indicate whether each stream has been
     exhausted. */
  def mapU[B](f: A => B): Stream[B] = Stream.unfold[B, Stream[A]](this) {
    case Cons(h, t) => Some((f(h()), t()))
    case Empty => None
  }

  def takeU(n: Int): Stream[A] = Stream.unfold[A, (Int, Stream[A])]((n, this)) {
    case (n, Cons(h, t)) if n > 0 => Some((h(), (n - 1, t())))
    case _ => None
  }

  def takeWhileU(p: A => Boolean): Stream[A] = Stream.unfold[A, Stream[A]](this) {
    case Empty => None
    case Cons(h, t) =>
      val head = h()
      if (p(head)) Some((head, t())) else None
  }

  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C]  = Stream.unfold((this, s2)) {
    case (Cons(h, t), Cons(h2, t2)) => Some((f(h(), h2()), (t(), t2())))
    case _ => None
  }

  def zip[B](s2: Stream[B]): Stream[(A,B)] =
    zipWith(s2)((_,_))

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = Stream.unfold((this, s2)) {
    case (Empty, Empty) => None
    case (Empty, Cons(h2, t2)) => Some(((None, Some(h2())), (Empty, t2())))
    case (Cons(h, t), Empty) => Some(((Some(h()), None), (t(), Empty)))
    case (Cons(h, t), Cons(h2, t2)) => Some(((Some(h()), Some(h2())), (t(), t2())))
  }

  /* Exercise 5.14
     Hard: Implement startsWith using functions you’ve written. It should check if one
     Stream is a prefix of another. For instance, Stream(1,2,3) startsWith Stream(1,2)
     would be true. */
  def startsWith[B >: A](s: Stream[B]): Boolean = zipAll(s).forAll {
    case (Some(v), Some(v2)) => v == v2
    case (_, None) => true
    case _ => false
  }

  /* Exercise 5.15
     Implement tails using unfold. For a given Stream, tails returns the Stream of suffixes
     of the input sequence, starting with the original Stream. For example, given
     Stream(1,2,3), it would return Stream(Stream(1,2,3), Stream(2,3), Stream(3), Stream()).*/
  def tails: Stream[Stream[A]] =
    Stream.unfold(this) {
      case Empty => None
      case s @ Cons(_, t) => Some((s, t()))
    }.append(Stream(Empty))

  def hasSubsequence[B >: A](s: Stream[B]): Boolean =
    tails exists (_ startsWith s)

  /* Exercise 5.16
     Hard: Generalize tails to the function scanRight, which is like a foldRight that
     returns a stream of the intermediate results. For example:
      scala> Stream(1,2,3).scanRight(0)(_ + _).toList
      res0: List[Int] = List(6,5,3,0)
     This example should be equivalent to the expression List(1+2+3+0, 2+3+0, 3+0,0).
     Your function should reuse intermediate results so that traversing a Stream with n
     elements always takes time linear in n. Can it be implemented using unfold? How, or
     why not? Could it be implemented using another function we’ve written?*/

  def scanRight[B](z: B)(f: (A, B) => B): Stream[B] = foldRight(Stream(z)) {
    case (v, s @ Cons(h, _)) => Stream.cons(f(v, h()), s)
  }

  // scanr via unfold. It's worse than O(n)
  def scanRightU[B](z: B)(f: (A, B) => B): Stream[B] = Stream.unfold(this) {
    case s @ Cons(_, t) => Some((s.foldRight(z)((v, acc) => f(v, acc)), t()))
    case Empty => None
  }.append(Stream(z))

  def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) =>
      val v = h()
      if (f(v)) Some(v) else t().find(f)
  }
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

  val ones: Stream[Int] = Stream.cons(1, ones)

  /* Exercise 5.8
     Generalize ones slightly to the function constant, which returns an infinite Stream of
     a given value. */
  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  /* Exercise 5.9
     Write a function that generates an infinite stream of integers,
     starting from n, then n + 1, n + 2, and so on. */
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  /* Exercise 5.10
     Write a function fibs that generates the infinite stream of Fibonacci numbers: 0, 1, 1,
     2, 3, 5, 8, and so on. */
  def fibs(): Stream[Int] = {
    def loop(prev: Int, next: Int): Stream[Int] = cons(prev, loop(next, prev + next))
    loop(0, 1)
  }

  /* Exercise 5.11
     Write a more general stream-building function called unfold. It takes an initial state,
     and a function for producing both the next state and the next value in the generated
     stream. */
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((v, s)) => cons(v, unfold(s)(f))
      case None => empty[A]
    }
  }

  /* Exercise 5.12
     Write fibs, from, constant, and ones in terms of unfold */
  def fibsU(): Stream[Int] = unfold((0, 1)) {case (prev, next) => Some(prev, (next, prev + next))}

  def fromU(n: Int): Stream[Int] = unfold(n)(s => Some(s, s + 1))

  def constantU[A](a: A): Stream[A] = unfold(a)(s => Some(s, s))

  val onesU: Stream[Int] = unfold(1)(s => Some(s, s))
}