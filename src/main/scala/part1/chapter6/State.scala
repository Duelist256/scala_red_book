package part1.chapter6

case class State[S,+A](run: S => (A,S))

object State {
  /* TODO Exercise 6.10
     Generalize the functions unit, map, map2, flatMap, and sequence. Add them as methods
     on the State case class where possible. Otherwise you should put them in a State
     companion object. */
}