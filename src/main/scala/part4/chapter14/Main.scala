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
  }
}
