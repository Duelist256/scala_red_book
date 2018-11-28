package part3.chapter10


import java.util.concurrent.Executors

import org.scalatest.{FunSpec, Matchers}

import scala.concurrent.Await
import scala.concurrent.duration._

class MonoidTest extends FunSpec with Matchers {

  describe("MonoidTest") {
    describe("Exercise 10.6: foldLeft and foldRight via foldMap") {
      it("should sum all elements") {
        val list = List(1, 2, 3, 4, 5)
        Monoid.foldLeft(list)(0)(_ + _) shouldEqual list.sum
        Monoid.foldRight(list)(0)(_ + _) shouldEqual list.sum
      }
      it("should multiply all elements") {
        val list = List(1, 2, 3, 4, 5)
        Monoid.foldLeft(list)(1)(_ * _) shouldEqual list.product
        Monoid.foldRight(list)(1)(_ * _) shouldEqual list.product
      }
      it("should concatenata all elements") {
        val list = List("Hello", ", ", "World", "!")
        Monoid.foldLeft(list)("")(_ + _) shouldEqual list.foldLeft("")(_ + _)
        Monoid.foldRight(list)("")(_ + _) shouldEqual list.foldRight("")(_ + _)
      }
    }
    describe("Exercise 10.8: parFoldMap") {
      it("should sum all elements") {
        val is = IndexedSeq("1", "2", "3", "4", "5")

        val ec = Executors.newCachedThreadPool()

        val f = Monoid.parFoldMap(is, MonoidInstances.intAddition)(_.toInt)(ec)
        val v = Monoid.foldMapV(is, MonoidInstances.intAddition)(_.toInt)

        f.get() shouldEqual v
      }
      it("should multiply all elements") {
        val is = IndexedSeq("1", "2", "3", "4", "5")

        val ec = Executors.newCachedThreadPool()

        val f = Monoid.parFoldMap(is, MonoidInstances.intMultiplication)(_.toInt)(ec)
        val v = Monoid.foldMapV (is, MonoidInstances.intMultiplication)(_.toInt)

        f.get() shouldEqual v
      }
      it("should concatenate all elements") {
        val is = IndexedSeq(1, 2, 3, 4, 5)
        val is2 = IndexedSeq(1, 2, 3, 4, 5)

        val ec = Executors.newCachedThreadPool()

        val f = Monoid.parFoldMap(is, MonoidInstances.stringMonoid)(_.toString)(ec)
        val v = Monoid.foldMapV (is2, MonoidInstances.stringMonoid)(_.toString)

        f.get() shouldEqual v
      }
      it("should concatenate all lists") {
        val is = IndexedSeq(Array(1, 2), Array(3, 4), Array(5, 6), Array(7, 8), Array(9, 10))

        val ec = Executors.newCachedThreadPool()

        val f = Monoid.parFoldMap(is, MonoidInstances.listMonoid[Int])(_.toList)(ec)
        val v = Monoid.foldMapV (is, MonoidInstances.listMonoid[Int])(_.toList)

        f.get() shouldEqual v
      }
    }
    describe("Exercise 10.9: isOrdered") {
      it("should return whether IndexedSeq ordered or not") {
        val emptyIs = IndexedSeq()
        val singleIs = IndexedSeq(1)
        val is = IndexedSeq(1, 2, 3, 4, 5)
        val is2 = IndexedSeq(1, 3, 3, 4, 5)
        val is3 = IndexedSeq(5, 1, 3, 3, 4, 5)
        val is4 = IndexedSeq(5, 4, 3, 2, 4, 5)
        Monoid.isOrdered(emptyIs) shouldBe true
        Monoid.isOrdered(singleIs) shouldBe true
        Monoid.isOrdered(is) shouldBe true
        Monoid.isOrdered(is2) shouldBe true
        Monoid.isOrdered(is3) shouldBe false
        Monoid.isOrdered(is4) shouldBe false
      }
    }
  }
}
