package part1

package object chapter6 {
  type Rand[+A] = RNG => (A, RNG)
}