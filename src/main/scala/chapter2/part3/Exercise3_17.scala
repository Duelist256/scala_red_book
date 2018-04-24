package chapter2.part3

object Exercise3_17 extends App {
  import List.turnDoubleIntoString

  val list = List(2.7, 11.11, 2.28, 0.0)

  assert(turnDoubleIntoString(list) == List("2.7", "11.11", "2.28", "0.0"))
}