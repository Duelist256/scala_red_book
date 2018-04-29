package chapter2.part3

object Exercise3_28 extends App {
  import Tree.map

  val tree = Branch(Leaf(-1), Branch(Leaf(2), Branch(Leaf(-3), Branch(Leaf(4), Leaf(-5)))))

  assert(map(tree)(e => e * 2) == Branch(Leaf(-2), Branch(Leaf(4), Branch(Leaf(-6), Branch(Leaf(8), Leaf(-10))))))
  assert(map(tree)(e => e % 2 == 0) == Branch(Leaf(false), Branch(Leaf(true), Branch(Leaf(false), Branch(Leaf(true), Leaf(false))))))
  assert(map(tree)(e => e * 1.5) == Branch(Leaf(-1.5), Branch(Leaf(3.0), Branch(Leaf(-4.5), Branch(Leaf(6.0), Leaf(-7.5))))))
}