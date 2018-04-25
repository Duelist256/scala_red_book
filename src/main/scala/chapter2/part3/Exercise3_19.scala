package chapter2.part3

object Exercise3_19 extends App {

  import List.filter

  val list = List(1, -1, -2, 2, 3, 4, -7, 5)
  assert(filter(list)(_ > 0) == List(1, 2, 3, 4, 5))

  val list2 = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

  assert(filter(list2)(_ % 2 == 1) == List(1, 3, 5, 7, 9))
}