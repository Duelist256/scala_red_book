package part3.chapter11

import part1.chapter5.{Cons, Empty, Stream}
import part1.chapter6.State
import part2.chapter7.Par
import part2.chapter8.Gen

trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)(a => unit(f(a)))
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a, b)))

  /* Exercise 11.3
     The sequence and traverse combinators should be pretty familiar to you by now, and
     your implementations of them from various prior chapters are probably all very similar.
     Implement them once and for all on Monad[F]. */
  def sequence[A](lma: List[F[A]]): F[List[A]] = lma match {
    case x :: xs => map2(x, sequence(xs))(_ :: _)
    case Nil => unit(Nil)
  }

  def traverse[A,B](la: List[A])(f: A => F[B]): F[List[B]] = sequence(la map f)

  /* Exercise 11.4
     Implement replicateM.*/
  def replicateM[A](n: Int, ma: F[A]): F[List[A]] = sequence(List.fill(n)(ma))

  def product[A,B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))

  /* Exercise 11.6
     Hard: Here’s an example of a function we haven’t seen before. Implement the function
     filterM. It’s a bit like filter, except that instead of a function from A => Boolean, we
     have an A => F[Boolean]. (Replacing various ordinary functions like this with the
     monadic equivalent often yields interesting results.) Implement this function, and
     then think about what it means for various data types. */
  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
    ms match {
      case Nil => unit(Nil)
      case x :: xs => flatMap(f(x))(
        if (_) map(filterM(xs)(f))(list => x :: list)
        else filterM(xs)(f)
      )
    }

  /* Exercise 11.7
     Implement the Kleisli composition function compose. */
  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  /* Exercise 11.8
     Hard: Implement flatMap in terms of compose. It seems that we’ve found another
     minimal set of monad combinators: compose and unit. */
  def flatMapViaCompose[A, B](fa: F[A])(f: A => F[B]): F[B] =
    compose[F[A], A, B](identity, a => f(a))(fa)

  /* Exercise 11.12
     There’s a third minimal set of monadic combinators: map, unit, and join. Implement
     join in terms of flatMap. */
  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(identity)

  /* Exercise 11.13
     Implement either flatMap or compose in terms of join and map. */
  def flatMapViaJoin[A, B](fa: F[A])(f: A => F[B]): F[B] = join(map(fa)(f))
  def composeViaJoin[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => join(map(f(a))(g))
}

object Monad {
  val genMonad = new Monad[Gen] {
    override def unit[A](a: => A): Gen[A] = Gen.unit(a)
    override def flatMap[A, B](fa: Gen[A])(f: A => Gen[B]): Gen[B] = fa flatMap f
  }

  /* Exercise 11.1
     Write monad instances for Par, Parser, Option, Stream, and List.*/
  val parMonad = new Monad[Par] {
    override def unit[A](a: => A): Par[A] = Par.unit(a)
    override def flatMap[A, B](fa: Par[A])(f: A => Par[B]): Par[B] = Par.flatMap(fa)(f)
  }

  val parserMonad = ???


  val optionMonad = new Monad[Option] {
    override def unit[A](a: => A): Option[A] = Some(a)
    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa flatMap f
  }

  val streamMonad = new Monad[Stream] {
    override def unit[A](a: => A): Stream[A] = Cons(() => a, () => Empty)
    override def flatMap[A, B](fa: Stream[A])(f: A => Stream[B]): Stream[B] = fa flatMap f
  }

  val listMonad = new Monad[List] {
    override def unit[A](a: => A): List[A] = List(a)
    override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa flatMap f
  }

  /* Exercise 11.2
     Hard: State looks like it would be a monad too, but it takes two type arguments and
     you need a type constructor of one argument to implement Monad. Try to implement a
     State monad, see what issues you run into, and think about possible solutions. We’ll
     discuss the solution later in this chapter. */
  def stateMonad[S] = new Monad[({type f[x] = State[S,x]})#f] {
    def unit[A](a: => A): State[S,A] = State(s => (a, s))
    def flatMap[A,B](st: State[S,A])(f: A => State[S,B]): State[S,B] =
      st flatMap f
  }

  /* Exercise 12.5
     Write a monad instance for Either. */
  def eitherMonad[E] = new Monad[({type f[x] = Either[E, x]})#f] {
    override def unit[A](a: => A): Either[E, A] = Right(a)
    override def flatMap[A, B](fa: Either[E, A])(f: A => Either[E, B]): Either[E, B] =
      fa.flatMap(f)
  }
}