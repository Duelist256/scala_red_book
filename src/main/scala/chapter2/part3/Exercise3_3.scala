package chapter2.part3

object Exercise3_3 extends App {
  import List.{setHead, tail}

  val list = List(1, 2, 3)

  println(setHead(5, list))
  println(setHead(5, tail(list)))
  println(setHead(5, Nil))
}