package part1.chapter8

import part1.chapter6.RNG
import part1.chapter8.Prop.{TestCases, MaxSize}
import part1.chapter5.Stream

case class Prop(run: (MaxSize,TestCases,RNG) => Result) {

  /* Exercise 8.9
     Now that we have a representation of Prop, implement && and || for composing Prop
     values. Notice that in the case of failure we don’t know which property was responsible,
     the left or the right. Can you devise a way of handling this, perhaps by allowing
     Prop values to be assigned a tag or label which gets displayed in the event of a failure?*/

  def ||(p: Prop): Prop = Prop { (max, n, rng) =>
    (this.run(max, n, rng), p.run(max, n, rng)) match {
      case (Passed, _) => Passed
      case (_, Passed) => Passed
      case (Falsified(failure, successes), Falsified(failure2, successes2)) =>
        Falsified(s"$failure/$failure2", successes + successes2)
    }
  }

  def &&(p: Prop): Prop = Prop { (max, n, rng) =>
    (this.run(max, n, rng), p.run(max, n, rng)) match {
      case (Passed, Passed) => Passed
      case (f: Falsified, Passed) => f
      case (Passed, f: Falsified) => f
      case (Falsified(failure, successes), Falsified(failure2, successes2)) =>
        Falsified(s"$failure/$failure2", successes + successes2)
    }
  }

}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) =>
        try {
          if (f(a)) Passed else Falsified(a.toString, i)
        } catch {
          case e: Exception => Falsified(buildMsg(a, e), i)
        }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g.forSize(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max,n,rng) =>
      val casesPerSize = (n + (max - 1)) / max
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop { (max, _, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max,n,rng)
  }
}