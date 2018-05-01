package part1.chapter2

object Exercise2_1 extends App {
  def fib(n: Int): Int = {
    def loop(first: Int, second: Int, n: Int): Int = {
      if (n == 0) first
      else loop(second, first + second, n - 1)
    }
    loop(0, 1, n)
  }

  println(fib(0))
  println(fib(1))
  println(fib(5))
  println(fib(7))
  println(fib(40))
}
