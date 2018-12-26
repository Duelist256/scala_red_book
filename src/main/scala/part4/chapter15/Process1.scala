package part4.chapter15

trait Process1[F[_],O] {
  import Process1._
  def onHalt(f: Throwable => Process1[F,O]): Process1[F,O] = this match {
    case Halt(e) => Try(f(e))
    case Emit(h, t) => Emit(h, t.onHalt(f))
    case Await(req,recv) => Await(req, recv andThen (_.onHalt(f)))
  }
  def ++(p: => Process1[F,O]): Process1[F,O] =
    this.onHalt {
      case End => p
      case err => Halt(err)
    }

  def flatMap[O2](f: O => Process1[F,O2]): Process1[F,O2] =
    this match {
      case Halt(err) => Halt(err)
      case Emit(o, t) => Try(f(o)) ++ t.flatMap(f)
      case Await(req,recv) =>
        Await(req, recv andThen (_ flatMap f))
    }
}

object Process1 {
  case class Await[F[_],A,O](req: F[A],
                             recv: Either[Throwable, A] => Process1[F,O]) extends Process1[F,O]
  case class Emit[F[_],O](head: O,
                          tail: Process1[F,O]) extends Process1[F,O]
  case class Halt[F[_],O](err: Throwable) extends Process1[F,O]
  case object End extends Exception
  case object Kill extends Exception


  def Try[F[_],O](p: => Process1[F,O]): Process1[F,O] =
    try p
    catch { case e: Throwable => Halt(e) }

  def await[F[_],A,O](
                       req: F[A])(
                       recv: Either[Throwable,A] => Process1[F,O]): Process1[F,O] =
    Await(req, recv)
}