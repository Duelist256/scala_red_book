package chapter2.part3

object Exercise3_15 extends App {
  import List.flatten

  val listOfLists = List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))

  assert(flatten(listOfLists) == List(1, 2, 3, 4, 5, 6, 7, 8, 9))
}