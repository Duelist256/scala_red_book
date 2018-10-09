package part1.chapter6

import org.scalatest.{FunSpec, Matchers}

class RNGTest extends FunSpec with Matchers {
  describe("RNGTest") {
    val MAX = 10000
    describe("Exercise 6.1: nonNegativeInt") {
      it("should return non negative Int") {
        val initialValue = 1
        val initialRng: RNG = SimpleRNG(initialValue)
        val (_, list) = (initialValue to MAX).foldLeft((initialRng, List[Int]())) {
          case ((rng, acc), _) =>
            val (value, newRng) = RNG.nonNegativeInt(rng)
            (newRng, value :: acc)
        }
        list.forall(v => v >= 0 && v <= Int.MaxValue) shouldBe true
      }
    }
    describe("Exercise 6.2: double") {
      it("should return double from 0 inclusive to 1 exclusive") {
        val initialValue = 1
        val initialRng: RNG = SimpleRNG(initialValue)
        val (_, list) = (initialValue to MAX).foldLeft((initialRng, List[Double]())) {
          case ((rng, acc), _) =>
            val (value, newRng) = RNG.double(rng)
            (newRng, value :: acc)
        }
        list.forall(v => v >= 0.0 && v < 1.0) shouldBe true
      }
    }
    describe("Exercise 6.3: intDouble") {
      it("should return random int and random double from 0 inclusive to 1 exclusive") {
        val initialValue = 1
        val initialRng: RNG = SimpleRNG(initialValue)
        val (_, list) = (initialValue to MAX).foldLeft((initialRng, List[(Int, Double)]())) {
          case ((rng, acc), _) =>
            val (value, newRng) = RNG.intDouble(rng)
            (newRng, value :: acc)
        }
        list.forall {
          case (i, d) => (i >= Int.MinValue && i <= Int.MaxValue) && (d >= 0.0 && d < 1.0)
        } shouldBe true
      }
    }
    describe("Exercise 6.3: doubleInt") {
      it("should return random double from 0 inclusive to 1 exclusive and random int ") {
        val initialValue = 1
        val initialRng: RNG = SimpleRNG(initialValue)
        val (_, list) = (initialValue to MAX).foldLeft((initialRng, List[(Double, Int)]())) {
          case ((rng, acc), _) =>
            val (value, newRng) = RNG.doubleInt(rng)
            (newRng, value :: acc)
        }
        list.forall {
          case (d, i) => (d >= 0.0 && d < 1.0) && (i >= Int.MinValue && i <= Int.MaxValue)
        } shouldBe true
      }
    }
    describe("Exercise 6.3: double3") {
      it("should return Tuple3 of doubles from 0 inclusive to 1 exclusive") {
        val initialValue = 1
        val initialRng: RNG = SimpleRNG(initialValue)
        val (_, list) = (initialValue to MAX).foldLeft((initialRng, List[(Double, Double, Double)]())) {
          case ((rng, acc), _) =>
            val (value, newRng) = RNG.double3(rng)
            (newRng, value :: acc)
        }
        list.forall {
          case (d, d2, d3) => (d >= 0.0 && d < 1.0) && (d2 >= 0.0 && d2 < 1.0) && (d3 >= 0.0 && d3 < 1.0)
        } shouldBe true
      }
    }
    describe("Exercise 6.4: ints") {
      it("should return list of random ints") {
        val initialValue = 1
        val initialRng: RNG = SimpleRNG(initialValue)
        val (list, _) = RNG.ints(MAX)(initialRng)
        list.forall(i => i >= Int.MinValue && i <= Int.MaxValue) shouldBe true
      }
    }
    describe("Exercise 6.5: doubleM") {
      it("should return double from 0 inclusive to 1 exclusive") {
        val initialValue = 1
        val initialRng: RNG = SimpleRNG(initialValue)
        val (_, list) = (initialValue to MAX).foldLeft((initialRng, List[Double]())) {
          case ((rng, acc), _) =>
            val (value, newRng) = RNG.doubleM(rng)
            (newRng, value :: acc)
        }
        list.forall(v => v >= 0.0 && v < 1.0) shouldBe true
      }
    }
    describe("Exercise 6.6: map2") {
      it("should return random int and random double from 0 inclusive to 1 exclusive") {
        val initialValue = 1
        val initialRng: RNG = SimpleRNG(initialValue)
        val (_, list) = (initialValue to MAX).foldLeft((initialRng, List[(Int, Double)]())) {
          case ((rng, acc), _) =>
            val (value, newRng) = RNG.map2(RNG.int, RNG.double)((_, _))(rng)
            (newRng, value :: acc)
        }
        list.forall {
          case (i, d) => (i >= Int.MinValue && i <= Int.MaxValue) && (d >= 0.0 && d < 1.0)
        } shouldBe true
      }
    }
    describe("Exercise 6.7: sequence") {
      it("should return list of random ints") {
        val initialValue = 1
        val initialRng: RNG = SimpleRNG(initialValue)

        val ints: Int => Rand[List[Int]] = count => RNG.sequence[Int](List.fill[Rand[Int]](count)(_.nextInt))
        val (list, _)= ints(MAX / 10)(initialRng)
        list.forall(i => i >= Int.MinValue && i <= Int.MaxValue) shouldBe true
      }
    }
    describe("Exercise 6.8: flatMap and nonNegativeLessThan") {
      it("should return non negative number that less than 10") {
        val initialValue = 1
        val initialRng: RNG = SimpleRNG(initialValue)
        val n = 10

        val (v, _) = RNG.nonNegativeLessThan(n)(initialRng)
        (v >= 0 && v < n) shouldBe true
      }
    }
    describe("Exercise 6.9: mapFM") {
      it("should map random integer N to string as partN") {
        val initialValue = 1
        val initialRng: RNG = SimpleRNG(initialValue)
        val (i, _) = RNG.int(initialRng)

        val (result, _) = RNG.mapFM[Int, String](RNG.int)(n => s"part$n")(initialRng)
        result shouldEqual s"part$i"
      }
    }
    describe("Exercise 6.9: map2FM") {
      it("should return random int and random double from 0 inclusive to 1 exclusive") {
        val initialValue = 1
        val initialRng: RNG = SimpleRNG(initialValue)
        val (_, list) = (initialValue to MAX).foldLeft((initialRng, List[(Int, Double)]())) {
          case ((rng, acc), _) =>
            val (value, newRng) = RNG.map2(RNG.int, RNG.double)((_, _))(rng)
            (newRng, value :: acc)
        }
        list.forall {
          case (i, d) => (i >= Int.MinValue && i <= Int.MaxValue) && (d >= 0.0 && d < 1.0)
        } shouldBe true
      }
    }
  }
}
