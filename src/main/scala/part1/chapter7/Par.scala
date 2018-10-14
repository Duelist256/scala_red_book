package part1.chapter7

import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}


object Par {
  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit): A = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  /* TODO Exercise 7.3
     Hard: Fix the implementation of map2 so that it respects the contract of timeouts on
     Future */

  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call: A = a(es).get()
    })

  /* Exercise 7.4
     This API already enables a rich set of operations. Here’s a simple example: using
     lazyUnit, write a function to convert any function A => B to one that evaluates its
     result asynchronously. */
  def asyncF[A,B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def map[A,B](pa: Par[A])(f: A => B): Par[B] = map2(pa, unit(()))((a,_) => f(a))
  def sortPar(parList: Par[List[Int]]): Par[List[Int]] = map(parList)(_.sorted)

  /* Exercise 7.5
     Hard: Write this function, called sequence. No additional primitives are required. Do
     not call run.*/
  def sequence[A](ps: List[Par[A]]): Par[List[A]] = ps match {
    case Nil => unit(Nil)
    case x :: xs => map2(x, sequence(xs))(_ :: _)
  }

  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  /* Exercise 7.6
     Implement parFilter, which filters elements of a list in parallel.*/
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] =
    map(parMap[A, List[A]](as)(a => if (f(a)) List(a) else Nil))(_.flatten)
}