package part1.chapter6

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {

  /* Exercise 6.1
     Write a function that uses RNG.nextInt to generate a random integer between 0 and
     Int.maxValue (inclusive). Make sure to handle the corner case when nextInt returns
     Int.MinValue, which doesn’t have a non-negative counterpart. */
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (v, newRng) = rng.nextInt
    val newValue =
      if (v == Int.MinValue) v + 1 else v
    (math.abs(newValue), newRng)
  }

  /* Exercise 6.2
     Write a function to generate a Double between 0 and 1, not including 1. Note: You can
     use Int.MaxValue to obtain the maximum positive integer value, and you can use
     x.toDouble to convert an x: Int to a Double. */
  def double(rng: RNG): (Double, RNG) = {
    val (v, newRng) = nonNegativeInt(rng)
    val newValue = (if (v == Int.MaxValue) v - 1 else v) / Int.MaxValue
    (newValue.toDouble, newRng)
  }

  /* Exercise 6.3
     Write functions to generate an (Int, Double) pair, a (Double, Int) pair, and a
     (Double, Double, Double) 3-tuple. You should be able to reuse the functions you’ve
     already written. */
  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((i, d), rng3)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (iD, rng2) = intDouble(rng)
    (iD.swap, rng2)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d, d2, d3), rng4)
  }

  /* Exercise 6.4
     Write a function to generate a list of random integers. */
  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    (1 to count).foldLeft((List[Int](), rng)) {
      case ((acc, currentRng), _) =>
        val (value, newRng) = currentRng.nextInt
        (value :: acc, newRng)
    }

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  /* EXERCISE 6.5
     Use map to reimplement double in a more elegant way. See exercise 6.2. */
  def doubleM: Rand[Double] = map(nonNegativeInt)(v => ((if (v == Int.MaxValue) v - 1 else v) / Int.MaxValue).toDouble)

  /* Exercise 6.6
     Write the implementation of map2 based on the following signature. This function
     takes two actions, ra and rb, and a function f for combining their results, and returns
     a new action that combines them: */
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] = both(int, double)

  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  /* Exercise 6.7
     Hard: If you can combine two RNG transitions, you should be able to combine a whole
     list of them. Implement sequence for combining a List of transitions into a single
     transition. Use it to reimplement the ints function you wrote before. For the latter,
     you can use the standard library function List.fill(n)(x) to make a list with x
     repeated n times. */
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs match {
      case Nil => unit[List[A]](List())
      case x :: xs => map2(x, sequence(xs))(_ :: _)
    }
}