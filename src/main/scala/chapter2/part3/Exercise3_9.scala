package chapter2.part3

object Exercise3_9 extends App {
  import List.length
  assert(length(List(1, 2, 3)) == 3 )
  assert(length(List(1, 2, 3, 4, 5)) == 5 )
  assert(length(List()) == 0 )
}