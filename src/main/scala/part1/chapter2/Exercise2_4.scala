package part1.chapter2

object Implemented {
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    a: A => b: B => f(a, b)
  }

  val concatLength: String => String => Int =
    curry[String, String, Int]((s1, s2) => (s1 + s2).length)
}

object Exercise2_4 extends App {

  import part1.chapter2.Implemented.concatLength

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  val unc = uncurry(concatLength)
  println(unc("Keck", "Lol"))
}
