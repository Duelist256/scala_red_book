package part2

import java.util.concurrent.ExecutorService
import java.util.concurrent.Future

package object chapter7 {
  type Par[A] = ExecutorService => Future[A]
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)
}
