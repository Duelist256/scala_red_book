package part4.chapter14

trait ST[S, A] { self =>
  protected def run(s: S): (A, S)
  def map[B](f: A => B): ST[S, B] = new ST[S, B] {
    protected def run(s: S): (B, S) = {
      val (a, s2) = self.run(s)
      (f(a), s2)
    }
  }

  def flatMap[B](f: A => ST[S, B]): ST[S, B] = new ST[S, B] {
    protected def run(s: S): (B, S) = {
      val (a, s2) = self.run(s)
      f(a).run(s2)
    }
  }
}

object ST {
  def apply[S, A](a: => A): ST[S, A] = {
    lazy val memo = a
    s: S => (memo, s)
  }

  def runST[A](st: RunnableST[A]): A =
    st.apply[Unit].run(())._1
}
