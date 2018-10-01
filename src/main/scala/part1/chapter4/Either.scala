package part1.chapter4

sealed trait Either[+E, +A] {

  /* Exercise 4.6
     Implement versions of map, flatMap, orElse, and map2 on Either that operate on the
     Right value. */

  def map[B](f: A => B): Either[E, B] =
    this match {
      case Left(v) => Left(v)
      case Right(v) => Right(f(v))
    }


  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this match {
      case Left(v) => Left(v)
      case Right(v) => f(v)
    }


  def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] =
    this match {
      case Left(_) => b
      case right => right
    }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    flatMap[EE, C](a => b.map(bb => f(a, bb)))
}

object Either {
  /* Exercise 4.7
     Implement sequence and traverse for Either. These should return the first error
     that’s encountered, if there is one */
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es)(identity)

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as.foldRight[Either[E, List[B]]](Right(Nil))((e, acc) => f(e).map2(acc)((e, acc) => e :: acc))

  /* Exercise 4.8
     In this implementation, map2 is only able to report one error, even if both the name
     and the age are invalid. What would you need to change in order to report both errors?
     Would you change map2 or the signature of mkPerson? Or could you create a new data
     type that captures this requirement better than Either does, with some additional
     structure? How would orElse, traverse, and sequence behave differently for that
     data type? */

  case class Person(name: Name, age: Age)
  sealed case class Name(value: String)
  sealed case class Age(value: Int)

  def mkName(name: String): Either[String, Name] =
    if (name == "" || name == null) Left("Name is empty.")
    else Right(Name(name))

  def mkAge(age: Int): Either[String, Age] =
    if (age < 0) Left("Age is out of range.")
    else Right(Age(age))

  def mkPerson(name: String, age: Int): Either[String, Person] =
    mkName(name).map2(mkAge(age))((n, a) => Person(n, a))

  def mkPerson2(name: String, age: Int): Either[List[String], Person] =
    mkName(name) match {
      case Right(n) =>
        mkAge(age) match {
          case Right(a) => Right(Person(n, a))
          case Left(e) => Left(List(e))
        }
      case Left(e) =>
        mkAge(age) match {
          case Left(e2) => Left(List(e2, e))
          case _ => Left(List(e))
        }
    }
}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]