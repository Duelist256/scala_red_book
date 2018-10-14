package part1.chapter6

case class State[S,+A](run: S => (A,S)) {
  def map[B](f: A => B): State[S, B] = State(s => {
    val (a, s2) = run(s)
    (f(a), s2)
  })

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s2) = run(s)
    f(a).run(s2)
  })

  def map2[B, C](bstate: State[S, B])(f: (A, B) => C): State[S, C] = flatMap[C] (
    a => bstate.map[C](b => f(a, b))
  )
}

object State {
  /* Exercise 6.10
     Generalize the functions unit, map, map2, flatMap, and sequence. Add them as methods
     on the State case class where possible. Otherwise you should put them in a State
     companion object. */
  def unit[S, A](a: A): State[S, A] = State((a, _))

  def sequence[A, S](fs: List[State[S, A]]): State[S, List[A]] =
    fs match {
      case Nil => unit[S, List[A]](List())
      case x :: xs => x.map2(sequence(xs))(_ :: _)
    }

  def get[S]: State[S, S] = State(s => (s, s))
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()
}