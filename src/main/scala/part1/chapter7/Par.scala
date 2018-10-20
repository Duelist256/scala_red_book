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

  /* Exercise 7.11
     Implement choiceN and then choice in terms of choiceN. */
  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    ec => {
      val i = run(ec)(n).get()
      choices(i)(ec)
    }

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    ec => choiceN(map(cond)(if (_) 0 else 1))(List(t, f))(ec)

  /* Exercise 7.12
     There’s still something rather arbitrary about choiceN. The choice of List seems
     overly specific. Why does it matter what sort of container we have? For instance, what
     if, instead of a list of computations, we have a Map of them:18 */
  def choiceMap[K,V](key: Par[K])(choices: Map[K,Par[V]]): Par[V] =
    ec => choices(key(ec).get())(ec)

  /* Exercise 7.13
     Implement this new primitive chooser, and then use it to implement choice and
     choiceN.*/
  def chooser[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] = ec =>
    choices(pa(ec).get())(ec)

  def choice2[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = chooser(cond)(if (_) t else f)

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = chooser(n)(choices(_))

  /* Exercise 7.14
     Implement join. Can you see how to implement flatMap using join? And can you
     implement join using flatMap? */
  def join[A](a: Par[Par[A]]): Par[A] = ec => a(ec).get()(ec)

  def flatMap[A,B](a: Par[A])(f: A => Par[B]): Par[B] = join(map(a)(f))

  def joinViaFM[A](a: Par[Par[A]]): Par[A] = flatMap(a)(identity)
}