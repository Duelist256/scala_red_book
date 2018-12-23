package part4.chapter15

import org.scalatest.{FunSpec, Matchers}
import part4.chapter15.Process._

class ProcessTest extends FunSpec with Matchers {
  describe("ProcessTest") {
    describe("Exercise 15.1: take") {
      it("should take no elements") {
        val stream = Stream(1, 2, 3)
        val actualList = take(0)(stream).toList
        val expectedList = List()
        actualList shouldEqual expectedList
      }
      it("should take first element") {
        val stream = Stream(1, 2, 3)
        val actualList = take(1)(stream).toList
        val expectedList = List(1)
        actualList shouldEqual expectedList
      }
      it("should take 2 elements") {
        val stream = Stream(1, 2, 3)
        val actualList = take(2)(stream).toList
        val expectedList = List(1, 2)
        actualList shouldEqual expectedList
      }
      it("should take 3 elements") {
        val stream = Stream(1, 2, 3)
        val actualList = take(3)(stream).toList
        val expectedList = List(1, 2, 3)
        actualList shouldEqual expectedList
      }
      it("should take all elements") {
        val stream = Stream(1, 2, 3)
        val actualList = take(4)(stream).toList
        val expectedList = stream.toList
        actualList shouldEqual expectedList
      }
    }
    describe("Exercise 15.1: drop") {
      it("should drop no elements") {
        val stream = Stream(1, 2, 3)
        val actualList = drop(0)(stream).toList
        val expectedList = stream.toList
        actualList shouldEqual expectedList
      }
      it("should drop first element") {
        val stream = Stream(1, 2, 3)
        val actualList = drop(1)(stream).toList
        val expectedList = List(2, 3)
        actualList shouldEqual expectedList
      }
      it("should drop 2 elements") {
        val stream = Stream(1, 2, 3)
        val actualList = drop(2)(stream).toList
        val expectedList = List(3)
        actualList shouldEqual expectedList
      }
      it("should drop 3 elements") {
        val stream = Stream(1, 2, 3)
        val actualList = drop(3)(stream).toList
        val expectedList = List()
        actualList shouldEqual expectedList
      }
      it("should drop all elements") {
        val stream = Stream(1, 2, 3)
        val actualList = drop(4)(stream).toList
        val expectedList = List()
        actualList shouldEqual expectedList
      }
    }
    describe("Exercise 15.1: takeWhile") {
      it("should take no elements") {
        val stream = Stream(1, 2, 3)
        val actualList = takeWhile[Int](_ < 0)(stream).toList
        val expectedList = List()
        actualList shouldEqual expectedList
      }
      it("should take first element") {
        val stream = Stream(1, 2, 3)
        val actualList = takeWhile[Int](_ == 1)(stream).toList
        val expectedList = List(1)
        actualList shouldEqual expectedList
      }
      it("should take 2 elements") {
        val stream = Stream(1, 2, 3)
        val actualList = takeWhile[Int](_ <= 2)(stream).toList
        val expectedList = List(1, 2)
        actualList shouldEqual expectedList
      }
      it("should take all elements") {
        val stream = Stream(1, 2, 3)
        val actualList = takeWhile[Int](_ <= 3)(stream).toList
        val expectedList = List(1, 2, 3)
        actualList shouldEqual expectedList
      }
    }
    describe("Exercise 15.1: dropWhile") {
      it("should drop no elements") {
        val stream = Stream(1, 2, 3)
        val actualList = dropWhile[Int](_ < 0)(stream).toList
        val expectedList = List(1, 2, 3)
        actualList shouldEqual expectedList
      }
      it("should drop first element") {
        val stream = Stream(1, 2, 3)
        val actualList = dropWhile[Int](_ < 2)(stream).toList
        val expectedList = List(2, 3)
        actualList shouldEqual expectedList
      }
      it("should drop 2 elements") {
        val stream = Stream(1, 2, 3)
        val actualList = dropWhile[Int](_ < 3)(stream).toList
        val expectedList = List(3)
        actualList shouldEqual expectedList
      }
      it("should drop all elements") {
        val stream = Stream(1, 2, 3)
        val actualList = dropWhile[Int](_ > 3)(stream).toList
        val expectedList = List(1, 2, 3)
        actualList shouldEqual expectedList
      }
    }
  }
}
