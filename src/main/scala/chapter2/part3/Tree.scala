package chapter2.part3

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  /* Exercise 3.25 */
  /* Write a function size that counts the number of nodes (leaves and branches) in a tree. */
  def size[A](tree: Tree[A]): Int = {
    tree match {
      case Leaf(_) => 1
      case Branch(left, right) => 1 + size(left) + size(right)
    }
  }

  /* Exercise 3.26 */
  /* Write a function maximum that returns the maximum element in a Tree[Int]. (Note:
  In Scala, you can use x.max(y) or x max y to compute the maximum of two integers x
  and y.) */
  def maximum(tree: Tree[Int]): Int = {
    tree match {
      case Leaf(v) => v
      case Branch(left, right) => maximum(left).max(maximum(right))
    }
  }

  /* Exercise 3.27 */
  /* TODO: Write a function depth that returns the maximum path length from the root of a tree
     to any leaf. */

  /* Exercise 3.28 */
  /* TODO: Write a function map, analogous to the method of the same name on List, that modifies
     each element in a tree with a given function. */

  /* Exercise 3.29 */
  /* TODO: Generalize size, maximum, depth, and map, writing a new function fold that abstracts
     over their similarities. Reimplement them in terms of this more general function. Can
     you draw an analogy between this fold function and the left and right folds for List? */
}