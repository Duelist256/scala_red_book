package part1.chapter3

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  /* Exercise 3.2 */
  def tail[A](xs: List[A]): List[A] = {
    xs match {
      case Cons(y, ys) => ys
      case Nil => Nil
    }
  }

  /* Exercise 3.3 */
  def setHead[A](elem: A, xs: List[A]): List[A] = {
    xs match {
      case Cons(y, ys) => Cons(elem, ys)
      case Nil => Cons(elem, Nil)
    }
  }

  /* Exercise 3.4 */
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n == 0 || l == Nil) l
    else drop(tail(l), n - 1)
  }

  /* Exercise 3.5 */
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Cons(x, xs) if f(x) => dropWhile(xs, f)
      case _ => l
    }
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  /* Exercise 3.6 */
  def init[A](l: List[A]): List[A] = {
    def loop(l: List[A], acc: List[A]): List[A] = {
      l match {
        case Cons(x, Nil) => acc
        case Cons(x, xs) => loop(xs, append(acc, List(x)))
        case Nil => acc
      }
    }

    loop(l, Nil)
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]): Int =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]): Double =
    foldRight(ns, 1.0)(_ * _)


  /* Exercise 3.7 */
  /* Can product, implemented using foldRight, immediately halt the recursion and
     return 0.0 if it encounters a 0.0? Why or why not? Consider how any short-circuiting
     might work if you call foldRight with a large list. This is a deeper question that we’ll
     return to in chapter 5.

     Answer: I think it can't because of an implementation of foldRight. But maybe if foldLeft had an
     additional parameter as predicate, it would be possible
     */


  /* Exercise 3.8 */
  /* See what happens when you pass Nil and Cons themselves to foldRight, like this:
     foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)). What do you think this
     says about the relationship between foldRight and the data constructors of List?

     Answer: It returns the same list. A - is Int, B - is List[Int]. Function (A, B) => B produces list.
     The method returns B, i.e. List[Int]
     */

  /* Exercise 3.9 */
  /* Compute the length of a list using foldRight. */
  def length[A](as: List[A]): Int = foldRight(as, 0)((_, count) => count + 1)

  /* Exercise 3.10 */
  /* Our implementation of foldRight is not tail-recursive and will result in a StackOverflowError
     for large lists (we say it’s not stack-safe). Convince yourself that this is the
     case, and then write another general list-recursion function, foldLeft, that is
     tail-recursive, using the techniques we discussed in the previous chapter. Here is its signature
  */
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case x Cons xs => foldLeft(xs, f(z, x))(f)
    }
  }

  /* Exercise 3.11 */
  /* Write sum, product, and a function to compute the length of a list using foldLeft. */

  def sumFl(ints: List[Int]): Int = foldLeft(ints, 0)(_ + _)

  def productFl(ds: List[Double]): Double = foldLeft(ds, 1.0)(_ * _)

  def lengthFl[A](as: List[A]): Int = foldLeft[A, Int](as, 0)((count, _) => count + 1)

  /* Exercise 3.12 */
  /* Write a function that returns the reverse of a list (given List(1,2,3) it returns
     List(3,2,1)). See if you can write it using a fold.
  */
  def reverse[A](as: List[A]): List[A] = foldLeft(as, Nil: List[A])((acc, e) => Cons(e, acc))

  /* Exercise 3.13 */
  /* Hard: Can you write foldLeft in terms of foldRight? How about the other way
     around? Implementing foldRight via foldLeft is useful because it lets us implement
     foldRight tail-recursively, which means it works even for large lists without overflowing
     the stack.
  */

  def foldLeftFR[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    foldRight[A, B](as, z)((el, acc) => f(acc, el))

  def foldRightFL[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft[A, B](as, z)((acc, e) => f(e, acc))

  /* Exercise 3.14 */
  /* Implement append in terms of either foldLeft or foldRight. */
  def appendFR[A](list1: List[A], list2: List[A]): List[A] =
    foldRight[A, List[A]](list1, list2)((elem, list) => Cons(elem, list))

  /* Exercise 3.15 */
  /* Hard: Write a function that concatenates a list of lists into a single list. Its runtime
     should be linear in the total length of all lists. Try to use functions we have already
     defined.
  */
  def flatten[A](list: List[List[A]]): List[A] =
    foldRight(list, List[A]())(appendFR)

  /* Exercise 3.16 */
  /* Write a function that transforms a list of integers by adding 1 to each element.
     (Reminder: this should be a pure function that returns a new List!)*/
  def incEachElemOfList(list: List[Int]): List[Int] =
    foldRight(list, List[Int]())((elem, acc) => Cons(elem + 1, acc))

  /* Exercise 3.17 */
  /* Write a function that turns each value in a List[Double] into a String. You can use
     the expression d.toString to convert some d: Double to a String.*/
  def turnDoubleIntoString(list: List[Double]): List[String] =
    foldRight(list, List[String]())((elem, acc) => Cons(elem.toString, acc))

  /* Exercise 3.18 */
  /* Write a function map that generalizes modifying each element in a list while maintaining
     the structure of the list. Here is its signature:*/
  def map[A, B](as: List[A])(f: A => B): List[B] =
    foldRight(as, List[B]())((elem, acc) => Cons(f(elem), acc))

  /* Exercise 3.19 */
  /* Write a function filter that removes elements from a list unless they satisfy a given
     predicate. Use it to remove all odd numbers from a List[Int]. */
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, List[A]())((elem, acc) => if (f(elem)) Cons(elem, acc) else acc)

  /* Exercise 3.20 */
  /* Write a function flatMap that works like map except that the function given will return
     a list instead of a single result, and that list should be inserted into the final resulting
     list. Here is its signature: */
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    flatten(map(as)(f))

  /* For instance, flatMap(List(1,2,3))(i => List(i,i)) should result in List(1,1,2,2,3,3). */

  /* Exercise 3.21 */
  /* Use flatMap to implement filter. */
  def filterViaFM[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as){
      e => if(f(e)) List(e) else Nil
    }

  /* Exercise 3.22 */
  /* Write a function that accepts two lists and constructs a new list by adding corresponding
     elements. For example, List(1,2,3) and List(4,5,6) become List(5,7,9). */
  def sumEachElem(first: List[Int], second: List[Int]): List[Int] = {
      (first, second) match {
        case (_, Nil) | (Nil, _) => Nil
        case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, sumEachElem(xs, ys))
      }
  }

  /* Exercise 3.23 */
  /* Generalize the function you just wrote so that it’s not specific to integers or addition.
     Name your generalized function zipWith.*/
  def zipWith[A](first: List[A], second: List[A])(f: (A, A) => A): List[A] = {
    (first, second) match {
      case (_, Nil) | (Nil, _) => Nil
      case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys)(f))
    }
  }

  /* Exercise 3.24 */
  /* Hard: As an example, implement hasSubsequence for checking whether a List contains
  another List as a subsequence. For instance, List(1,2,3,4) would have
  List(1,2), List(2,3), and List(4) as subsequences, among others. You may have
  some difficulty finding a concise purely functional implementation that is also efficient.
  That’s okay. Implement the function however comes most naturally. We’ll
  return to this implementation in chapter 5 and hopefully improve on it. Note: Any
  two values x and y can be compared for equality in Scala using the expression x == y. */
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {

    def loop1(sup: List[A], sub: List[A]): Boolean =
    (sup, sub) match {
      case (Nil, list2) if list2 != Nil => false
      case (_, Nil) => true
      case (Cons(x, xs), Cons(y, ys)) => if (x == y) loop1(xs, ys) else false
    }

    def loop2(sup: List[A], sub: List[A]): Boolean = {
      (sup, sub) match {
        case (_, Nil) => true
        case (Nil, _) => false
        case (list1, list2) => if(!loop1(list1, list2)) loop2(tail(list1), list2) else true
      }
    }

    loop2(sup, sub)
  }
}