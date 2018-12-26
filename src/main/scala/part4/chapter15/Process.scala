package part4.chapter15

import part3.chapter11.Monad
import part4.chapter13.IO

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

  def |>[O2](p2: Process[O,O2]): Process[I,O2] = {
    p2 match {
      case Halt() => Halt()
      case Emit(head, tail) => Emit(head, this |> tail)
      case Await(recv) => this match {
        case Emit(head2, tail2) => tail2 |> recv(Some(head2))
        case Await(recv2) => Await(recv2 andThen (_ |> p2 ))
        case Halt() => Halt() |> recv(None)
      }
    }
  }

  def ++(p: => Process[I,O]): Process[I,O] = this match {
    case Halt() => p
    case Emit(h, t) => Emit(h, t ++ p)
    case Await(recv) => Await(recv andThen (_ ++ p))
  }

  def flatMap[O2](f: O => Process[I,O2]): Process[I,O2] = this match {
    case Halt() => Halt()
    case Emit(h, t) => f(h) ++ t.flatMap(f)
    case Await(recv) => Await(recv andThen (_ flatMap f))
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

  /* Exercise 15.1
     Implement take, which halts the Process after it encounters the given number of elements,
     and drop, which ignores the given number of arguments and then emits the
     rest. Also implement takeWhile and dropWhile, that take and drop elements as long
     as the given predicate remains true. */
  def take[I](n: Int): Process[I,I] = {
    def loop(n: Int): Process[I,I] = {
      Await[I,I] {
        case Some(i) if n > 0 => Emit(i, loop(n - 1))
        case _ => Halt()
      }
    }
    loop(n)
  }
  def drop[I](n: Int): Process[I,I] = {
    def loop(n: Int): Process[I,I] = {
      Await[I,I] {
        case Some(i) if n <= 0 => Emit(i, loop(n - 1))
        case Some(_) => loop(n - 1)
        case _ => Halt()
      }
    }
    loop(n)
  }
  def takeWhile[I](f: I => Boolean): Process[I,I] = {
    Await[I,I] {
      case Some(i) if f(i) => Emit(i, takeWhile(f))
      case _ => Halt()
    }
  }
  def dropWhile[I](f: I => Boolean): Process[I,I] = {
    Await[I,I] {
      case Some(i) if f(i) => dropWhile(f)
      case Some(i) => Emit(i, dropWhile(f))
      case _ => Halt()
    }

  }

  /* Exercise 15.2
     Implement count. It should emit the number of elements seen so far. For instance,
     count(Stream("a", "b", "c")) should yield Stream(1, 2, 3) (or Stream(0, 1, 2, 3),
     your choice). */
  def count[I]: Process[I,Int] = {
    def go(acc: Int): Process[I, Int] =
      Await {
        case Some(d) => Emit(acc, go(1 + acc))
        case None => Emit(acc)
      }
    go(0)
  }

  /* Exercise 15.3
     Implement mean. It should emit a running average of the values seen so far. */
  def mean: Process[Double,Double] = {
    def go(sum: Double, count: Int): Process[Double, Double] =
      Await {
        case Some(v) if count == 0 => Emit(0.0, go(sum + v, 1))
        case Some(v) => Emit(sum / count, go(sum + v, count + 1))
        case None if count == 0 => Emit(0.0)
        case None => Emit(sum / count)
      }
    go(0.0, 0)
  }

  def monad[I]: Monad[({ type f[x] = Process[I,x]})#f] =
    new Monad[({ type f[x] = Process[I,x]})#f] {
      def unit[O](o: => O): Process[I,O] = Emit(o)
      def flatMap[O,O2](p: Process[I,O])(
        f: O => Process[I,O2]): Process[I,O2] =
        p flatMap f
    }

  def processFile[A,B](f: java.io.File, p: Process[String, A], z: B)(g: (B, A) => B): IO[B] = IO {
    @annotation.tailrec
    def go(ss: Iterator[String], cur: Process[String, A], acc: B): B =
      cur match {
        case Halt() => acc
        case Await(recv) =>
          val next = if (ss.hasNext) recv(Some(ss.next))
          else recv(None)
          go(ss, next, acc)
        case Emit(h, t) => go(ss, t, g(acc, h))
      }
    val s = io.Source.fromFile(f)
    try go(s.getLines, p, z)
    finally s.close
  }

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

    val takeResult = take(2)(Stream(1, 2, 3))
    println(takeResult.toList)
  }
}