package chapter2.part3

object Exercise3_4 extends App {
  import List.drop

  val list = List(1, 2, 3, 4, 5)

  println(drop(list, 1))
  println(drop(list, 2))
  println(drop(list, 3))
  println(drop(list, 4))
  println(drop(Nil, 4))

}
