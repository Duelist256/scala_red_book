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
    val newValue = if (v == Int.MaxValue) v - 1 else v
    ((v / Int.MaxValue).toDouble, newRng)
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
}