package part4.chapter14

trait RunnableST[A] {
  def apply[S]: ST[S, A]
}
