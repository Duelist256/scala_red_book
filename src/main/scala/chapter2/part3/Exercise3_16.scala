package chapter2.part3

object Exercise3_16 extends App {
  import List.incEachElemOfList

  val list = List(1, 2, 3, 4)

  assert(incEachElemOfList(list) == List(2, 3, 4, 5))
}