package part1.chapter3

object Exercise3_1 extends App {
  import List.sum

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x: Int, Cons(y: Int, Cons(3, Cons(4, _)))) => x + y
    case Cons(h: Int, t: Cons[Int]) => h + sum(t)
    case _ => 101
  }

  assert(x == 3)
}