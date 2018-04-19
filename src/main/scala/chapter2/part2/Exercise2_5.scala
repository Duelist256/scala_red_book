package chapter2.part2

object Exercise2_5 extends App {
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    a: A => f(g(a))
  }

  val divideBy2AndThenSquare = compose[Int, Int, Int](x => x * x, x => x / 2)
  println(divideBy2AndThenSquare(10))


  // default compose and andThen example
  val square: Int => Int = (x: Int) => x * x
  val divideBy2: Int => Int = (x: Int) => x / 2

  val sqDivBy2 = square.andThen(divideBy2)
  val divBy2Sq = square.compose(divideBy2)

  println(sqDivBy2(10))
  println(divBy2Sq(10))
}
