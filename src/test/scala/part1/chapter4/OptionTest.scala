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
      it("filter: Should return None if there is a value and it satisfies predicate, otherwise return this") {
        val some2 = Some(8)
        some.filter(_ % 2 == 1) shouldEqual None
        some2.filter(_ % 2 == 1) shouldEqual some2
        None.filter(e => e != e) shouldEqual None
      }
    }
  }
}