package part1.chapter8

trait Prop {
  def check: Boolean

  /* Exercise 8.3
     Assuming the following representation of Prop, implement && as a method of Prop. */
  def &&(p: Prop): Boolean = check && p.check
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
}