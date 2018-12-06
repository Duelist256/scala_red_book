package part3.chapter12

import part3.chapter11.Functor

trait Applicative[F[_]] extends Functor[F] {
  // primitive combinators
  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]
  def unit[A](a: => A): F[A]

  // derived combinators
  def map[A, B](fa: F[A])(f: A => B): F[B] =
    map2(fa, unit(()))((a, _) => f(a))

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))

  /* Exercise 12.1
     Transplant the implementations of as many combinators as you can from Monad to
     Applicative, using only map2 and unit, or methods implemented in terms of them. */
  def sequence[A](fas: List[F[A]]): F[List[A]] = traverse(fas)(identity)

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    if (n <= 0) unit(Nil)
    else map2(fa, replicateM(n - 1, fa))(_ :: _)

  def product[A,B](fa: F[A], fb: F[B]): F[(A,B)] = map2(fa, fb)((_, _))
}

object Applicative {
  val streamApplicative = new Applicative[Stream] {
    override def unit[A](a: => A): Stream[A] = Stream.continually(a)
    override def map2[A, B, C](a: Stream[A], b: Stream[B])(f: (A, B) => C): Stream[C] =
      a zip b map f.tupled

    /* Exercise 12.4
       Hard: What is the meaning of streamApplicative.sequence? Specializing the signature
       of sequence to Stream, we have this: */
    def sequence2[A](a: List[Stream[A]]): Stream[List[A]] =
      a match {
        case Nil => unit(Nil)
        case x :: xs => map2(x, sequence(xs))(_ :: _)
      }
  }

  /* Exercise 12.6
     Write an Applicative instance for Validation that accumulates errors in Failure.
     Note that in the case of Failure there’s always at least one error, stored in head. The
     rest of the errors accumulate in the tail. */
  def validationApplicative[E] = new Applicative[({type f[x] = Validation[E, x]})#f] {
    override def unit[A](a: => A): Validation[E, A] = Success(a)
    override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] =
      (fa, fb) match {
        case (Failure(e, t), Failure(e2, t2)) => Failure(e, t ++ (e2 +: t2))
        case (Success(a), f @ Failure(_, _)) => f
        case (f @ Failure(_, _), Success(a)) => f
        case (Success(a), Success(a2)) => Success(f(a, a2))
      }
  }
}