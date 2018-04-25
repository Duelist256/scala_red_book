package chapter2.part3

object Exercise3_23 extends App {

  import List.zipWith

  val list1 = List(1, 2, 3, 4, 5)
  val list2 = List(2, 3, 4, 5, 6, 3)

  assert(zipWith(list1, list2)(_ + _) == List(3, 5, 7, 9, 11))

  val list3 = List("Ba", "Or", "Pine", "Ap", "Water", "Nil")
  val list4 = List("nana", "ange", "apple", "ple", "melon")
  assert(zipWith(list3, list4)(_ + _)
    == List("Banana", "Orange", "Pineapple", "Apple", "Watermelon"))
}