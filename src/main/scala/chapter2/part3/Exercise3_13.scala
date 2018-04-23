package chapter2.part3

object Exercise3_13 extends App {
  import List.{foldLeftFR, foldRightFL, foldLeft, foldRight}

  val list = List(1, 2, 3, 4, 5)

  val f: (Int, Int) => Int = _ - _

  assert(foldLeftFR(list, 0)(f) == foldLeft(list, 0)(f))
  assert(foldRight(list, 0)(f) == foldRightFL(list, 0)(f))

}