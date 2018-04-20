package chapter2.part3

object Exercise3_2 extends App {

  import chapter2.part3.List.tail

  val list = List(1, 2, 3)
  println(tail(list))
  println(tail(tail(list)))
  println(tail(tail(tail(list))))
  println(tail(tail(tail(tail(list)))))
}