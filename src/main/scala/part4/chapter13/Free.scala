package part4.chapter13

import part3.chapter11.Monad

import scala.annotation.tailrec

sealed trait Free[F[_], A] {
  def flatMap[B](f: A => Free[F, B]): Free[F, B] = FlatMap(this, f)
  def map[B](f: A => B): Free[F, B] = flatMap(f andThen (Return(_)))
}
case class Return[F[_], A](a: A) extends Free[F, A]
case class Suspend[F[_], A](resume: F[A]) extends Free[F, A]
case class FlatMap[F[_], A, B](sub: Free[F, A], f: A => Free[F, B]) extends Free[F, B]

object Free {
  /* Exercise 13.1
     Free is a monad for any choice of F. Implement map and flatMap methods on the
     Free trait, and give the Monad instance for Free[F,_].10 */
  def freeMonad[F[_]]= new Monad[({type f[a] = Free[F,a]})#f] {
    override def unit[A](a: => A): Free[F, A] = Return(a)
    override def flatMap[A, B](fa: Free[F, A])(f: A => Free[F, B]): Free[F, B] = fa flatMap f
  }

  /* Exercise 13.2
     Implement a specialized tail-recursive interpreter, runTrampoline, for running a
     Free[Function0,A]. */
  @tailrec
  def runTrampoline[A](a: Free[Function0,A]): A = a match {
    case Return(r) => r
    case Suspend(s) => s()
    case FlatMap(sub, f) => sub match {
      case Return(r) => runTrampoline(f(r))
      case Suspend(r) => runTrampoline(f(r()))
      case FlatMap(sub2, g) => runTrampoline(sub2.flatMap(s => g(s) flatMap f))
    }
  }

  /* Exercise 13.3
     Implement a `Free` interpreter which works for any `Monad`*/
  def run[F[_],A](a: Free[F,A])(implicit F: Monad[F]): F[A] = step(a) match {
    case Return(a) => F.unit(a)
    case Suspend(r) => r
    case FlatMap(Suspend(r), f) => F.flatMap(r)(a => run(f(a)))
    case _ => sys.error("Impossible, since `step` eliminates these cases")
  }

  // return either a `Suspend`, a `Return`, or a right-associated `FlatMap`
  @annotation.tailrec
  def step[F[_],A](a: Free[F,A]): Free[F,A] = a match {
    case FlatMap(FlatMap(x, f), g) => step(x flatMap (a => f(a) flatMap g))
    case FlatMap(Return(x), f) => step(f(x))
    case _ => a
  }
}