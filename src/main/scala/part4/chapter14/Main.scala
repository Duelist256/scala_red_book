package part4.chapter14

object Main {
  def main(args: Array[String]): Unit = {
    val p = new RunnableST[(Int, Int)] {
      def apply[S]: ST[S, (Int, Int)] = for {
        r1 <- STRef(1)
        r2 <- STRef(2)
        x <- r1.read
        y <- r2.read
        _ <- r1.write(y + 1)
        _ <- r2.write(x + 1)
        a <- r1.read
        b <- r2.read
      } yield (a, b)
    }

    println(ST.runST(p))

    val bypass = new RunnableST[STRef[_, Int]] {
      override def apply[S]: ST[S, STRef[_, Int]] = for {
        r1 <- STRef(1)
      } yield r1
    }

    /* TODO Exercise 14.2
       Write the purely functional versions of partition and qs. */
    def partition[S](arr: STArray[S,Int], n: Int, r: Int, pivot: Int): ST[S,Int] = ???
    def qs[S](a: STArray[S,Int], n: Int, r: Int): ST[S,Unit] = ???
    def quicksort(xs: List[Int]): List[Int] =
      if (xs.isEmpty) xs else ST.runST(new RunnableST[List[Int]] {
        def apply[S] = for {
          arr <- STArray.fromList(xs)
          size <- arr.size
          _ <- qs(arr, 0, size - 1)
          sorted <- arr.freeze
        } yield sorted
      })
  }
}
