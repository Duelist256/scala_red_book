package part2.chapter8

import part1.chapter6.{RNG, State}

case class Gen[A](sample: State[RNG,A]) {
  /* Exercise 8.6
     Implement flatMap, and then use it to implement this more dynamic version of
     listOfN. Put flatMap and listOfN in the Gen class. */
  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(f(_).sample))
  def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(Gen.listOfN(_, this))

  /* Exercise 8.10
     Implement helper functions for converting Gen to SGen. You can add this as a method
     on Gen.*/
  def unsized: SGen[A] = SGen(_ => this)

  /* TODO Exercise 8.11
     Not surprisingly, SGen at a minimum supports many of the same operations as Gen,
     and the implementations are rather mechanical. Define some convenience functions
     on SGen that simply delegate to the corresponding functions on Gen.5 */
}

object Gen {
  /* Exercise 8.4
     Implement Gen.choose using this representation of Gen. It should generate integers in
     the range start to stopExclusive. Feel free to use functions you’ve already written. */
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))

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

  /* Exercise 8.7
     Implement union, for combining two generators of the same type into one, by pulling
     values from each generator with equal likelihood. */
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(if (_) g1 else g2)

  /* TODO Exercise 8.8
     Implement weighted, a version of union that accepts a weight for each Gen and generates
     values from each Gen with probability proportional to its weight. */
  def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] = ???

  /* Exercise 8.12
     Implement a listOf combinator that doesn’t accept an explicit size. It should return an
     SGen instead of a Gen. The implementation should generate lists of the requested size. */
  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(listOfN(_, g))

  /* Exercise 8.13
     Define listOf1 for generating nonempty lists, and then update your specification of
     max to use this generator */
  def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen(v => listOfN(v max 1, g))
}