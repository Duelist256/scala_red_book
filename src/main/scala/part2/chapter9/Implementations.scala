package part2.chapter9
import scala.util.matching.Regex

object Implementations {


  type Parser[+A] = Location => Result[A]

  object FirstParser extends Parsers[Parser] {
    override def run[A](p: Parser[A])(input: String): Either[ParseError, A] = ???

    override implicit def regex(r: Regex): Parser[String] = ???

    override implicit def string(s: String): Parser[String] = ???

    override def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A] = s => s1(s) match {
      case Failure(e, false) => s2(s)
      case r => r
    }

    override def slice[A](a: Parser[A]): Parser[String] = ???

    override def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] =
      s => p(s) match {
        case Success(a, n) =>
          f(a)(s.advanceBy(n))
            .addCommit(n != 0)
            .advanceSuccess(n)
        case e: Failure => e
      }

    override def label[A](msg: String)(p: Parser[A]): Parser[A] =
      p(_).mapError(_.label(msg))

    override def scope[A](msg: String)(p: Parser[A]): Parser[A] =
      loc => p(loc).mapError(_.push(loc, msg))

    override def attempt[A](p: Parser[A]): Parser[A] = s => p(s).uncommit

    override def errorLocation(e: ParseError): Location = ???

    override def errorMessage(e: ParseError): String = ???
  }
}