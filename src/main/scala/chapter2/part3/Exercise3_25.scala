package chapter2.part3

object Exercise3_25 extends App {
  import Tree.size

  val tree5 = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
  val tree7 = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))

  assert(size(tree5) == 5)
  assert(size(tree7) == 7)
}