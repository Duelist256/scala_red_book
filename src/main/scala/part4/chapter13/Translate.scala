package part4.chapter13

trait Translate[F[_], G[_]] {
  def apply[A](f: F[A]): G[A]
}
