package chapter2.part3

object Exercise3_18 extends App {
  import List.map

  val list = List(2.7, 11.11, 2.28, 0.0)
  val list2 = List("Hello", "World", "Whatcha", "doin")

  assert(map(list)(_ * 2.0) == List(5.4, 22.22, 4.56, 0.0))
  assert(map(list2)(_.reverse) == List("olleH", "dlroW", "ahctahW", "niod"))
}