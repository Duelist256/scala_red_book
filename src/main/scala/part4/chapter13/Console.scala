package part4.chapter13

import part2.chapter7.Par
import part3.chapter11.Monad
import part4.chapter13.Free.step

import scala.io.StdIn

sealed trait Console[A] {
  def toPar: Par[A]
  def toThunk: () => A
  def toReader: ConsoleReader[A]
}

case object ReadLine extends Console[Option[String]] {
  override def toPar: Par[Option[String]] = Par.lazyUnit(run)
  override def toThunk: () => Option[String] = () => run
  override def toReader: ConsoleReader[Option[String]] = ConsoleReader(s => Some(s))

  def run: Option[String] =
    try Some(StdIn.readLine())
    catch { case e: Exception => None }

}

case class PrintLine(line: String) extends Console[Unit] {
  override def toPar: Par[Unit] = Par.lazyUnit(println(line))
  override def toThunk: () => Unit = () => println(println(line))

  override def toReader: ConsoleReader[Unit] = ConsoleReader(s => println(s))
}


object Console {

  implicit val function0Monad = new Monad[Function0] {
    def unit[A](a: => A) = () => a
    def flatMap[A,B](a: Function0[A])(f: A => Function0[B]) =
      () => f(a())()
  }
  implicit val parMonad = new Monad[Par] {
    def unit[A](a: => A) = Par.unit(a)
    def flatMap[A,B](a: Par[A])(f: A => Par[B]) =
      Par.fork { Par.flatMap(a)(f) }
  }

  val consoleToFunction0 =
    new (Console ~> Function0) { def apply[A](a: Console[A]) = a.toThunk }
  val consoleToPar =
    new (Console ~> Par) { def apply[A](a: Console[A]) = a.toPar }
  val consoleToReader = new (Console ~> ConsoleReader) {
    def apply[A](a: Console[A]) = a.toReader
  }

  type ConsoleIO[A] = Free[Console, A]

  def runFree[F[_],G[_],A](free: Free[F,A])(t: F ~> G)(
    implicit G: Monad[G]): G[A] =
    step(free) match {
      case Return(a) => G.unit(a)
      case Suspend(r) => t(r)
      case FlatMap(Suspend(r),f) => G.flatMap(t(r))(a => runFree(f(a))(t))
      case _ => sys.error("Impossible; `step` eliminates these cases")
    }

  def runConsoleFunction0[A](a: Free[Console,A]): () => A =
    runFree[Console,Function0,A](a)(consoleToFunction0)
  def runConsolePar[A](a: Free[Console,A]): Par[A] =
    runFree[Console,Par,A](a)(consoleToPar)
  def runConsoleReader[A](io: ConsoleIO[A]): ConsoleReader[A] =
    runFree[Console,ConsoleReader,A](io)(consoleToReader)

  def readLn: ConsoleIO[Option[String]] = Suspend(ReadLine)
  def printLn(line: String): ConsoleIO[Unit] = Suspend(PrintLine(line))

  def main(args: Array[String]): Unit = {
    val f1: Free[Console, Option[String]] = for {
      _ <- printLn("I can only interact with the console.")
      ln <- readLn
    } yield ln

    runConsoleFunction0(f1)
  }
}