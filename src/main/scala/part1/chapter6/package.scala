package part1

package object chapter6 {
  type State[S,+A] = S => (A,S)
  type Rand[+A] = State[RNG, A]
}