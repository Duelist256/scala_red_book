package chapter2.part3

object Exercise3_26 extends App {
  import Tree.maximum

  val tree5 = Branch(Branch(Leaf(17), Leaf(-2)), Leaf(3))
  val tree7 = Branch(Branch(Leaf(-1), Leaf(2)), Branch(Leaf(31), Leaf(4)))

  assert(maximum(tree5) == 17)
  assert(maximum(tree7) == 31)
}