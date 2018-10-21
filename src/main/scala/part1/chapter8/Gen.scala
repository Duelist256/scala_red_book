package part1.chapter8

import part1.chapter6.{RNG, SimpleRNG, State}

case class Gen[A](sample: State[RNG,A])

object Gen {
  /* Exercise 8.4
     Implement Gen.choose using this representation of Gen. It should generate integers in
     the range start to stopExclusive. Feel free to use functions you’ve already written. */
  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    def foo(rng: RNG): (Int, RNG) = {
      val (v, rng2) = rng.nextInt
      if (v >= start && v < stopExclusive) (v, rng2) else foo(rng2)
    }
    Gen(State(foo))
  }

  /* Exercise 8.5
     Let’s see what else we can implement using this representation of Gen. Try implementing
     unit, boolean, and listOfN. */

  def unit[A](a: => A): Gen[A] = Gen(State(RNG.unit(a)))

  def boolean: Gen[Boolean] = Gen(State(rng => {
    val (i, rng2) = rng.nextInt
    (i > 0, rng2)
  }))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen[List[A]](
    State(initialRng =>
      (1 to n).foldRight((List[A](), initialRng)) {
        case (_, (list, rng)) =>
          val (v, rng2)  = g.sample.run(rng)
          (v :: list, rng2)
      }
    )
  )
}