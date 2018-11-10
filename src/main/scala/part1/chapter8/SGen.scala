package part1.chapter8

case class SGen[+A](forSize: Int => Gen[A])