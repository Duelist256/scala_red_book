package chapter2.part3

object Exercise3_21 extends App {

  import List.filterViaFM

  val list = List(1, -1, -2, 2, 3, 4, -7, 5)
  assert(filterViaFM(list)(_ > 0) == List(1, 2, 3, 4, 5))

  val list2 = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

  assert(filterViaFM(list2)(_ % 2 == 1) == List(1, 3, 5, 7, 9))
}