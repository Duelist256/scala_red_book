package part1.chapter8

case class SGen[A](forSize: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = forSize(n)
}