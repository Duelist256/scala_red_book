package part1.chapter8

import part1.chapter6.RNG
import part1.chapter8.Prop.TestCases

case class Prop(run: (TestCases, RNG) => Result) {

  /* Exercise 8.9
     Now that we have a representation of Prop, implement && and || for composing Prop
     values. Notice that in the case of failure we don’t know which property was responsible,
     the left or the right. Can you devise a way of handling this, perhaps by allowing
     Prop values to be assigned a tag or label which gets displayed in the event of a failure?*/

  def ||(p: Prop): Prop = Prop { (n, rng) =>
    (this.run(n, rng), p.run(n, rng)) match {
      case (Passed, _) => Passed
      case (_, Passed) => Passed
      case (Falsified(failure, successes), Falsified(failure2, successes2)) =>
        Falsified(s"$failure/$failure2", successes + successes2)
    }
  }

  def &&(p: Prop): Prop = Prop { (n, rng) =>
    (this.run(n, rng), p.run(n, rng)) match {
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
}