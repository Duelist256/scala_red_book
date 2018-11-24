package part2.chapter9

trait Result[+A] {
  def mapError(f: ParseError => ParseError): Result[A] = this match {
    case Failure(e, isCommited) => Failure(f(e), isCommited)
    case _ => this
  }

  def uncommit: Result[A] = this match {
    case Failure(e, true) => Failure(e, isCommited = false)
    case _ => this
  }

  def addCommit(isCommitted: Boolean): Result[A] = this match {
    case Failure(e,c) => Failure(e, c || isCommitted)
    case _ => this
  }

  def advanceSuccess(n: Int): Result[A] = this match {
    case Success(a, m) => Success(a, m + n)
    case _ => this
  }
}
case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
case class Failure(get: ParseError, isCommited: Boolean) extends Result[Nothing]