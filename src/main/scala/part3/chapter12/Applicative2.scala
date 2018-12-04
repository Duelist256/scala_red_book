package part3.chapter12

import part3.chapter11.Functor

/* Exercise 12.2
   Hard: The name applicative comes from the fact that we can formulate the Applicative
   interface using an alternate set of primitives, unit and the function apply, rather than
   unit and map2. Show that this formulation is equivalent in expressiveness by defining
   map2 and map in terms of unit and apply. Also establish that apply can be implemented
   in terms of map2 and unit. */
trait Applicative2[F[_]] extends Functor[F] {
  def apply[A,B](fab: F[A => B])(fa: F[A]): F[B] = map2(fa, fab)((a, f) => f(a))
  def unit[A](a: => A): F[A]
  def map[A,B](fa: F[A])(f: A => B): F[B] = apply(unit(f))(fa)
  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A,B) => C): F[C] =
    apply(apply(unit(f.curried))(fa))(fb)

  /* Exercise 12.3
     The apply method is useful for implementing map3, map4, and so on, and the pattern
     is straightforward. Implement map3 and map4 using only unit, apply, and the curried
     method available on functions. */
  def map3[A,B,C,D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
    apply(apply(apply(unit(f.curried))(fa))(fb))(fc)
  def map4[A,B,C,D,E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] =
    apply(apply(apply(apply(unit(f.curried))(fa))(fb))(fc))(fd)
}