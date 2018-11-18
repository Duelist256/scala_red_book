package part2.chapter7

import java.util.concurrent.Executors

import org.scalatest.{FunSpec, Matchers}
import part2._

class ParTest extends FunSpec with Matchers {
  describe("ParTest") {
    val ec = Executors.newCachedThreadPool()
    describe("Exercise 7.4: asyncF") {
      it("should multiple int by 3") {
        val par = Par.asyncF[Int, Int](_ * 3)(10)
        chapter7.run(ec)(par).get() shouldEqual 30
      }
    }
    describe("Exercise 7.5: sequence") {
      it("should convert list of pars to par of list") {
        val listOfPars = List(Par.unit(1), Par.unit(2), Par.unit(3), Par.unit(4), Par.unit(5))
        val parOfList = Par.sequence(listOfPars)
        chapter7.run(ec)(parOfList).get() shouldEqual List(1, 2, 3, 4, 5)
      }
    }
    describe("Exercise 7.6: parFilter") {
      it("should return par of list of odd numbers") {
        val list = List(1, 2, 3, 4, 5)
        val par = Par.parFilter(list)(_ % 2 == 1)
        chapter7.run(ec)(par).get() shouldEqual List(1, 3, 5)
      }
    }
  }
}
