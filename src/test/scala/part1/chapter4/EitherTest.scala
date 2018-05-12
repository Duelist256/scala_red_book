package part1.chapter4

import org.scalatest.{FunSpec, FunSuite, Matchers}

class EitherTest  extends FunSpec with Matchers {
  describe("Either test") {
    val right = Right(5)
    val left = Left(10)
    describe("Exercise 4.6: map") {
      it("should return Either with new value if it is Right") {
        right.map(_ * 2) shouldEqual Right(10)
        left.map(n => 2) shouldEqual left
      }
    }
    describe("Exercise 4.6: flatMap") {
      it("should return Either with new value if it is Right") {
        right.flatMap(v => Right(v * 2)) shouldEqual Right(10)
        left.flatMap(n => Right(2)) shouldEqual left
      }
    }
    describe("Exercise 4.6: orElse") {
      it("should return Either if Right or default value Left") {
        right.orElse(Right(20)) shouldEqual right
        left.orElse(Right(20)) shouldEqual Right(20)
      }
    }
    describe("Exercise 4.6: map2") {
      it("should combine two Rights or return first value if it is Left") {
        right.map2(Right(20))(_ * _) shouldEqual Right(100)
        right.map2(Left(20))((a, b) => a) shouldEqual Left(20)
        left.map2(Right(20))((n, b) => b) shouldEqual left
      }
    }
    describe("Exercise 4.7: sequence") {
      it("should return Either of List") {
        val list = List(Right(5), Right(10), Right(15))
        Either.sequence(list) shouldEqual Right(List(5, 10, 15))
        val list2 = List(Left(5), Right(10), Left(12), Right(15))
        Either.sequence(list2) shouldEqual Left(5)
      }
    }
    describe("Exercise 4.7: traverse") {
      it("should return Either of List") {
        val list = List(5, 10, 15)
        Either.traverse(list)(e => Right(e * 5)) shouldEqual Right(List(25, 50, 75))
        val list2 = List(Left(5), Right(10), Left(12), Right(15))
        Either.traverse(list2)(e => Left(e)) shouldEqual Left(Left(5))
      }
    }
  }
}
