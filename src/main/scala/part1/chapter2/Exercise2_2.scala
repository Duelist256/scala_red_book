package part1.chapter2

object Exercise2_2 extends App {
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def loop(i: Int, j: Int): Boolean = {
      if (j >= as.length) true
      else if (ordered(as(i), as(j))) loop(i + 1, j + 1)
      else false
    }
    loop(0, 1)
  }

  val arr = Array(1, 2, 3, 4)
  println(isSorted[Int](arr, _ < _))
  println(isSorted[Int](arr, _ > _))
  println

  val arr2 = Array(3, 2, 1, 0)
  println(isSorted[Int](arr2, _ < _))
  println(isSorted[Int](arr2, _ > _))
  println

  val arr3 = Array(1, 2)
  println(isSorted[Int](arr3, _ < _))

  val arr4 = Array(2, 0)
  println(isSorted[Int](arr4, _ < _))

  val arr5 = Array(0)
  println(isSorted[Int](arr5, _ < _))
}
