package part1.chapter4

import org.scalatest.{FunSpec, FunSuite, Matchers}

class OptionTest extends FunSpec with Matchers {
  describe("Option test") {
    describe("Exercise 4.1: map, flatMap, getOrElse, orElse, filter") {
      val some = Some(5)
      it("map: should return some with new element or return None") {
        some.map(_ * 5) shouldEqual Some(25)
        None.map(e => e) shouldEqual None
      }
      it("flatMap: should return some with new element or return None") {
        some.flatMap(v => Some(v * 5)) shouldEqual Some(25)
        None.flatMap(v => Some(v)) shouldEqual None
      }
      it("getOrElse: should return value or default value") {
        some.getOrElse(10) shouldEqual 5
        None.getOrElse(10) shouldEqual 10
      }
      it("orElse: should return this option if it is defined otherwise return second option") {
        some.orElse(Some(10)) shouldEqual some
        None.orElse(Some(10)) shouldEqual Some(10)
        None.orElse(None) shouldEqual None
      }
      it("filter: should return None if there is a value and it satisfies predicate, otherwise return this") {
        val some2 = Some(8)
        some.filter(_ % 2 == 1) shouldEqual some
        some2.filter(_ % 2 == 1) shouldEqual None
        None.filter(e => e != e) shouldEqual None
        some2.filter(_ > 8) shouldEqual None
      }
    }
    describe("Exercise 4.2: variance") {
      import Option.variance
      it("should return variance of given sequence") {
        variance(Seq(1, 2, 3, 4)) shouldEqual Some(1.25)
        variance(Seq(1, 1, 1, 1)) shouldEqual Some(0.0)
        variance(Seq()) shouldEqual None
      }
    }
    describe("Exercise 4.3: map2") {
      import Option.map2
      it("should return result of combining of two options") {
        map2(Some(2), Some(3))(_ * _) shouldEqual Some(6)
        map2(Some(210), Some(5))(_ / _) shouldEqual Some(42)
        map2(None, Some(3))((a, b) => b) shouldEqual None
        map2(Some(210), None)((a, b) => a) shouldEqual None
      }
    }
  }
}