package part2.chapter9

import part2.chapter8.{Gen, Prop}

trait Parsers[ParseError, Parser[+_]] { self =>
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]
  def char(c: Char): Parser[Char] = string(c.toString).map(_.head)
  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]

  /* Exercise 9.4
     Hard: Using map2 and succeed, implement the listOfN combinator from earlier. */
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n <= 0) succeed(List[A]())
    else p.map2[List[A], List[A]](listOfN(n - 1, p))(_ :: _)


  def many[A](P: Parser[A]): Parser[List[A]]
  def succeed[A](a: A): Parser[A] = string("").map(_ => a)
  def map[A, B](a: Parser[A])(f: A => B): Parser[B]
  def slice[A](a: Parser[A]): Parser[String]
  def many1[A](p: Parser[A]): Parser[List[A]] = p.map2(many(p))(_ :: _)
  def product[A, B](p: Parser[A], p2: Parser[B]): Parser[(A, B)]

  /* Exercise 9.1
     Using product, implement the now-familiar combinator map2 and then use this to
     implement many1 in terms of many. Note that we could have chosen to make map2
     primitive and defined product in terms of map2 as we’ve done in previous chapters.
     The choice is up to you.*/
  def map2[A,B,C](p: Parser[A], p2: Parser[B])(f: (A,B) => C): Parser[C] =
    product(p, p2).map[C](f.tupled)

  /* TODO Exercise 9.3
     Hard: Before continuing, see if you can define many in terms of or, map2, and succeed. */

  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] =
    ParserOps(f(a))


  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def many: Parser[List[A]] = self.many(p)
    def slice: Parser[String] = self.slice(p)
    def **[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)
    def map2[B, C](p2: Parser[B])(f: (A, B) => C): Parser[C] = self.map2(p, p2)(f)
  }

  object Laws {
    import Prop.forAll
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(identity))(in)

//    def succeedLaw[A](p: Parser[A])(in: Gen[String]): Prop =
//      forAll(in)(a => succeed(a) == Right(a))

    /* TODO Exercise 9.2
       Hard: Try coming up with laws to specify the behavior of product.*/
  }
}
