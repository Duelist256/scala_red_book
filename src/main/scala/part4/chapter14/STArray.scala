package part4.chapter14

sealed abstract class STArray[S, A](implicit manifest: Manifest[A]) {
  protected def value: Array[A]
  def size: ST[S, Int] = ST(value.length)

  def write(i: Int, a: A): ST[S, Unit] = new ST[S, Unit] {
    def run(s: S): (Unit, S) = {
      value(i) = a
      ((), s)
    }
  }

  def read(i: Int): ST[S, A] = ST(value(i))
  def freeze: ST[S, List[A]] = ST(value.toList)

  /* Exercise 14.1
     Add a combinator on STArray to fill the array from a Map where each key in the map
     represents an index into the array, and the value under that key is written to the array
     at that index. For example, xs.fill(Map(0->"a", 2->"b")) should write the value
     "a" at index 0 in the array xs and "b" at index 2. Use the existing combinators to write
     your implementation.*/
  def fill(xs: Map[Int,A]): ST[S,Unit] = xs.foldRight(ST[S, Unit](())){
    case ((idx, v), st) => st.flatMap(_ => write(idx, v))
  }
}

object STArray {
  def apply[S, A: Manifest](sz: Int, v: A): ST[S, STArray[S, A]] = ST(
    new STArray[S, A] {
      lazy val value: Array[A] = Array.fill(sz)(v)
    }
  )

  def fromList[S, A: Manifest](xs: List[A]): ST[S, STArray[S,A]] =
    ST(new STArray[S,A] {
      lazy val value = xs.toArray
    })
}