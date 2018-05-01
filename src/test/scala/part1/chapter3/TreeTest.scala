package part1.chapter3

import part1.chapter3.Tree._
import org.scalatest.{FunSpec, FunSuite, Matchers}

class TreeTest extends FunSpec with Matchers {
  describe("Tree test") {
    describe("Exercise 3.25: size") {
      it("should return size of the tree") {
        val tree5 = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
        val tree7 = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))
        Tree.size(tree5) shouldEqual 5
        Tree.size(tree7) shouldEqual 7
      }
    }
    describe("Exercise 3.26: maximum") {
      it("should return maximum element of the tree of integers") {
        val tree5 = Branch(Branch(Leaf(17), Leaf(-2)), Leaf(3))
        val tree7 = Branch(Branch(Leaf(-1), Leaf(2)), Branch(Leaf(31), Leaf(4)))
        maximum(tree5) shouldEqual 17
        maximum(tree7) shouldEqual 31
      }
    }
    describe("Exercise 3.27: size") {
      it("should return depth of the tree") {
        val tree2 = Branch(Leaf(-1), Leaf(2))
        val tree3 = Branch(Branch(Leaf(-1), Leaf(2)), Branch(Leaf(31), Leaf(4)))
        val tree5 = Branch(Leaf(-1), Branch(Leaf(-1), Branch(Leaf(-1), Branch(Leaf(-1), Leaf(2)))))
        depth(tree2) shouldEqual 2
        depth(tree3) shouldEqual 3
        depth(tree5) shouldEqual 5
      }
    }
    describe("Exercise 3.28: map") {
      it("should return new tree with mapped elements") {
        val tree = Branch(Leaf(-1), Branch(Leaf(2), Branch(Leaf(-3), Branch(Leaf(4), Leaf(-5)))))
        map(tree)(e => e * 2) shouldEqual Branch(Leaf(-2),
                                                 Branch(Leaf(4),
                                                        Branch(Leaf(-6),
                                                               Branch(Leaf(8), Leaf(-10)))))
        map(tree)(e => e % 2 == 0) shouldEqual Branch(Leaf(false),
                                                      Branch(Leaf(true),
                                                             Branch(Leaf(false),
                                                                    Branch(Leaf(true), Leaf(false)))))
        map(tree)(e => e * 1.5) shouldEqual Branch(Leaf(-1.5),
                                                   Branch(Leaf(3.0),
                                                          Branch(Leaf(-4.5),
                                                                 Branch(Leaf(6.0), Leaf(-7.5)))))
      }
    }
//    describe("Exercise 3.29: fold, sizeViaFold, maximumViaFold, depthViaFold and mapViaFold") {
//      it("fold: should return nonempty result") {
//        val tree = Branch(Branch(Leaf(-1), Leaf(2)), Branch(Leaf(31), Leaf(4)))
//        fold(tree, 0)(_ + _) shouldEqual 36
//        fold(tree, 1)(_ * _) shouldEqual -248
//        val treeOfStrings = Branch(Branch(Leaf("Hello, "), Leaf("World!")), Branch(Leaf(" Whatcha "), Leaf("doin?")))
//        fold(treeOfStrings, "")(_ + _) shouldEqual "Hello, World! Whatcha doin?"
//      }
//      it("sizeViaFold: should return nonempty result") {
//        val tree5 = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
//        val tree7 = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))
//        sizeViaFold(tree5) shouldEqual 3 // TODO: LOLKEK
//      }
//    }
  }
}
