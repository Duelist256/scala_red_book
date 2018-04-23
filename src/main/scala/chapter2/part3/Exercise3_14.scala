package chapter2.part3

object Exercise3_14 extends App {
  import List.appendFL

  val list1 = List(1, 2, 3, 4)
  val list2 = List(5, 6, 7)

  println(appendFL(list1, list2))
}