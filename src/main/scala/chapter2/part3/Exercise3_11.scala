package chapter2.part3

object Exercise3_11 extends App {
  import List.{sumFl, productFl, lengthFl}

  val ints = List(1, 2, 3, 4, 5)
  println(sumFl(ints))
  println(lengthFl(ints))

  val ds = List(2, 1.5, 3)
  println(productFl(ds))
  println(lengthFl(ds))

  val emptyI = List[Int]()
  val emptyD = List[Double]()

  println(lengthFl(emptyI))
  println(lengthFl(emptyD))
  println(productFl(emptyD))
  println(sumFl(emptyI))

}