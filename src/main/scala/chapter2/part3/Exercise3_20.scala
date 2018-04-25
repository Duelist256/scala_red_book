package chapter2.part3

object Exercise3_20 extends App {

  import List.flatMap

  val list = List(2, 3, 4, 5)

  assert(flatMap(list)(e => List(e, e * e)) == List(2, 4, 3, 9, 4, 16, 5, 25))
}