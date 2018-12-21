package part4.chapter15

sealed trait Process[I, O] {
  def apply(s: Stream[I]): Stream[O] = this match {
    case Halt() => Stream()
    case Await(recv) => s match {
      case h #:: t => recv(Some(h))(t)
      case xs => recv(None)(xs)
    }
    case Emit(h, t) => h #:: t(s)
  }

  def repeat: Process[I, O] = {
    def go(p: Process[I,O]): Process[I,O] = p match {
      case Halt() => go(this)
      case Await(recv) => Await {
        case None => recv(None)
        case i => go(recv(i))
      }
      case Emit(h, t) => Emit(h, go(t))
    }
    go(this)
  }
}

case class Emit[I, O](head: O, tail: Process[I, O] = Halt[I, O]()) extends Process[I, O]
case class Await[I, O](recv: Option[I] => Process[I, O]) extends Process[I, O]
case class Halt[I, O]() extends Process[I, O]

object Process {
  def liftOne[I, O](f: I => O): Process[I, O] =
    Await {
      case Some(i) => Emit(f(i))
      case None => Halt()
    }

  def lift[I, O](f: I => O): Process[I, O] = liftOne(f).repeat

  def filter[I](p: I => Boolean): Process[I, I] =
    Await[I, I] {
      case Some(i) if p(i) => Emit(i)
      case _ => Halt()
    }.repeat

  def sum: Process[Double,Double] = {
    def go(acc: Double): Process[Double,Double] =
      Await {
        case Some(d) => Emit(d+acc, go(d+acc))
        case None => Halt()
      }
    go(0.0)
  }

  /* TODO Exercise 15.1
     Implement take, which halts the Process after it encounters the given number of elements,
     and drop, which ignores the given number of arguments and then emits the
     rest. Also implement takeWhile and dropWhile, that take and drop elements as long
     as the given predicate remains true. */
  def take[I](n: Int): Process[I,I] = ???
  def drop[I](n: Int): Process[I,I] = ???
  def takeWhile[I](f: I => Boolean): Process[I,I] = ???
  def dropWhile[I](f: I => Boolean): Process[I,I] = ???

  /* TODO Exercise 15.2
     Implement count. It should emit the number of elements seen so far. For instance,
     count(Stream("a", "b", "c")) should yield Stream(1, 2, 3) (or Stream(0, 1, 2, 3),
     your choice). */
  def count[I]: Process[I,Int] = ???

  /* TODO Exercise 15.3
     Implement mean. It should emit a running average of the values seen so far. */
  def mean: Process[Double,Double] = ???

  def main(args: Array[String]): Unit = {
    val p: Process[Int, Int] = liftOne((x: Int) => x * 2)
    val xs: Stream[Int] = p(Stream(1, 2, 3))
    println(xs.toList)

    val p2: Process[Int, Int] = lift((x: Int) => x * 2)
    val ys: Stream[Int] = p(Stream(1, 2, 3))
    println(ys.toList)

    val units = Stream.continually(())
    val ones = lift((_:Unit) => 1)(units)
    println(ones)

    val even = filter((x: Int) => x % 2 == 0)
    val evens = even(Stream(1,2,3,4)).toList
    println(evens)

    val s = sum(Stream(1.0, 2.0, 3.0, 4.0)).toList
    println(s)
  }
}