package chapter2.part3

object Exercise3_24 extends App {

  import List.hasSubsequence

  val list = List(1, 2, 3, 4, 5)
  val subNil = Nil
  val sub1 = List(1)
  val sub2 = List(2)
  val sub3 = List(3)
  val sub4 = List(4)
  val sub5 = List(5)
  val sub12 = List(1, 2)
  val sub23 = List(2, 3)
  val sub34 = List(3, 4)
  val sub45 = List(4, 5)
  val sub123 = List(1, 2, 3)
  val sub234 = List(2, 3, 4)
  val sub345 = List(3, 4, 5)

  assert(hasSubsequence(list, subNil))
  assert(hasSubsequence(list, sub1))
  assert(hasSubsequence(list, sub2))
  assert(hasSubsequence(list, sub3))
  assert(hasSubsequence(list, sub4))
  assert(hasSubsequence(list, sub5))
  assert(hasSubsequence(list, sub12))
  assert(hasSubsequence(list, sub23))
  assert(hasSubsequence(list, sub34))
  assert(hasSubsequence(list, sub45))
  assert(hasSubsequence(list, sub123))
  assert(hasSubsequence(list, sub234))
  assert(hasSubsequence(list, sub345))


  val sub6 = List(6)
  val sub21 = List(2, 1)
  val sub32 = List(3, 2)
  val sub35 = List(3, 5)
  val sub51 = List(5, 1)
  val sub43 = List(4, 3)
  val sub413 = List(4, 1, 3)
  assert(!hasSubsequence(list, sub6))
  assert(!hasSubsequence(list, sub21))
  assert(!hasSubsequence(list, sub32))
  assert(!hasSubsequence(list, sub35))
  assert(!hasSubsequence(list, sub51))
  assert(!hasSubsequence(list, sub43))
  assert(!hasSubsequence(list, sub413))
}