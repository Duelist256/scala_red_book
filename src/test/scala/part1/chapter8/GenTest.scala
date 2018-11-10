package part1.chapter8

import org.scalatest.{FunSpec, Matchers}
import part1.chapter6.{RNG, SimpleRNG}

class GenTest extends FunSpec with Matchers {
  describe("GenTest") {
    describe("Exercise 8.4: choose") {
      it("should return random int from 1 to 9") {
        val initialValue = 1
        val initialRng: RNG = SimpleRNG(initialValue)
        val start = 1
        val stopExclusive = 10

        val (result, _) = Gen.choose(start, stopExclusive).sample.run(initialRng)
        (result >= start && result < stopExclusive) shouldBe true
      }
    }
    describe("Exercise 8.5: unit") {
      it("should return gen with specified value") {
        val initialValue = 1
        val initialRng: RNG = SimpleRNG(initialValue)
        val n = 10

        val (result, _) = Gen.unit(n).sample.run(initialRng)
        result shouldEqual n
      }
    }
    describe("Exercise 8.5: listOfN") {
      it("should return list of specified value of gen") {
        val initialValue = 1
        val initialRng: RNG = SimpleRNG(initialValue)
        val size = 10
        val value = 5
        val gen = Gen.unit(value)

        val (result, _) = Gen.listOfN(size, gen).sample.run(initialRng)
        result.size shouldEqual size
        result.forall(_ == value)
      }
    }
    describe("Exercise 8.6: flatMap") {
      it("should return gen of changed value") {
        val initialValue = 1
        val initialRng: RNG = SimpleRNG(initialValue)
        val value = 5
        val n = 10
        val expectedResult = value + n
        val gen = Gen.unit(value)

        val (result, _) = gen.flatMap(i => Gen.unit(i + n)).sample.run(initialRng)
        result shouldEqual expectedResult
      }
    }
    describe("Exercise 8.6: listOfN") {
      it("should return list of specified value of gen") {
        val initialValue = 1
        val initialRng: RNG = SimpleRNG(initialValue)
        val size = 10
        val genSize = Gen.unit(size)
        val value = 5
        val genValue = Gen.unit(value)

        val (result, _) = genValue.listOfN(genSize).sample.run(initialRng)
        result.size shouldEqual size
        result.forall(_ == value)
      }
    }
  }
}
