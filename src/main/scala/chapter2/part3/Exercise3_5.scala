package chapter2.part3

object Exercise3_5 extends App {

  import List.dropWhile

  val list = List(1, 3, 5, 7, 8)

  println(dropWhile[Int](list, _ <= 3))
  println(dropWhile[Int](list, _ % 2 == 1))
  println(dropWhile[Int](list, _ > 0))
}