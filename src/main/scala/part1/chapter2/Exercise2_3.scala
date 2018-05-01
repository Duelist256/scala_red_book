package part1.chapter2

object Exercise2_3 extends App {
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    a: A => b: B => f(a, b)
  }

  val c1 = curry[String, String, Int]((s1, s2) => (s1 + s2).length)
  val c2 = c1("hello")
  println(c1("keque")("keck"))
  println(c2("keque"))
}

