package chapter2.part3

object Exercise3_27 extends App {
  import Tree.depth

  val tree2 = Branch(Leaf(-1), Leaf(2))
  val tree3 = Branch(Branch(Leaf(-1), Leaf(2)), Branch(Leaf(31), Leaf(4)))

  val tree5 = Branch(Leaf(-1), Branch(Leaf(-1), Branch(Leaf(-1), Branch(Leaf(-1), Leaf(2)))))

  assert(depth(tree2) == 2)
  assert(depth(tree3) == 3)
  assert(depth(tree5) == 5)
}