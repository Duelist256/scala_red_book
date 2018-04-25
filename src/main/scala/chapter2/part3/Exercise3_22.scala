package chapter2.part3

object Exercise3_22 extends App {

  import List.sumEachElem

  val list1 = List(1, 2, 3, 4, 5)
  val list2 = List(2, 3, 4, 5, 6, 3)

  assert(sumEachElem(list1, list2) == List(3, 5, 7, 9, 11))

  val list3 = List(1, 2, 3, 4, 5, 7)
  val list4 = List(1)
  assert(sumEachElem(list3, list4) == List(2))
}