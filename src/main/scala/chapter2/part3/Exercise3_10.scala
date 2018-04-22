package chapter2.part3

object Exercise3_10 extends App {
  val i: Int = collection.immutable.List[Int]().foldLeft(0)(_ + _)
  println(i)

  import List.foldLeft
  val list = List(1, 2, 3, 4, 5)
  println(foldLeft(list, 0)(_ + _))
  println(foldLeft(list, 1)(_ * _))

  println(foldLeft(List(1, 2, 3), 1)(_ * _))
  println(foldLeft(List(3), 1)(_ + _))

  println(foldLeft(List[Int](), 1)(_ + _))
  println(foldLeft(List[Int](), 1)(_ * _))

}