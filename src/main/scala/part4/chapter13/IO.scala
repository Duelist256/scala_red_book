package part4.chapter13

import part3.chapter11.Monad

import scala.io.StdIn

sealed trait IO[A] { self =>
  def run: A
  def map[B](f: A => B): IO[B] =
    new IO[B] { def run = f(self.run)}
  def flatMap[B](f: A => IO[B]): IO[B] =
    new IO[B] { def run = f(self.run).run }

  def **[B](ioB: IO[B]): IO[(A, B)] =
    flatMap(a => ioB.map(b => (a, b)))
}

object IO extends Monad[IO] {
  def unit[A](a: => A): IO[A] = new IO[A] { def run: A = a }
  def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = fa flatMap f
  def apply[A](a: => A): IO[A] = unit(a)

  def ReadLine: IO[String] = IO { StdIn.readLine }
  def PrintLine(msg: String): IO[Unit] = IO { println(msg) }

  def converter: IO[Unit] = for {
    _ <- PrintLine("Enter a temperature in degrees Fahrenheit: ")
    d <- ReadLine.map(_.toDouble)
    _ <- PrintLine(fahrenheitToCelsius(d).toString)
  } yield ()

  def fahrenheitToCelsius(f: Double): Double = (f - 32) * 5.0 / 9.0

  val echo: IO[Unit] = ReadLine.flatMap(PrintLine)
  val readInt: IO[Int] = ReadLine.map(_.toInt)
  val readInts: IO[(Int, Int)] = readInt ** readInt
}