package part1.chapter5

import org.scalatest.{FunSpec, Matchers}

class StreamTest extends FunSpec with Matchers {
  describe("Stream test") {
    describe("Exercise 5.1: toList") {
      it("should convert stream of 5 elements to list") {
        val stream = Stream(1, 2, 3, 4, 5)
        val list = stream.toList
        list shouldEqual List(1, 2, 3, 4, 5)
      }
      it("should convert stream of 1 element to list") {
        val stream = Stream(1)
        val list = stream.toList
        list shouldEqual List(1)
      }
      it("should convert empty stream to list") {
        val stream = Stream.empty
        val list = stream.toList
        list shouldEqual List.empty
      }
    }
    describe("Exercise 5.2: take") {
      it("should take no elements from the stream") {
        val stream = Stream(1, 2, 3, 4, 5)
        stream.take(-1).toList shouldEqual List()
      }
      it("should take no elements from the stream 2") {
        val stream = Stream(1, 2, 3, 4, 5)
        stream.take(0).toList shouldEqual List()
      }
      it("should take first element from the stream") {
        val stream = Stream(1, 2, 3, 4, 5)
        stream.take(1).toList shouldEqual List(1)
      }
      it("should take first 3 elements from the stream") {
        val stream = Stream(1, 2, 3, 4, 5)
        stream.take(3).toList shouldEqual List(1, 2, 3)
      }
      it("should take first 5 elements from the stream") {
        val stream = Stream(1, 2, 3, 4, 5)
        stream.take(5).toList shouldEqual List(1, 2, 3, 4, 5)
      }
      it("should take all elements from the stream") {
        val stream = Stream(1, 2, 3, 4, 5)
        stream.take(55).toList shouldEqual List(1, 2, 3, 4, 5)
      }
    }
    describe("Exercise 5.3: takeWhile") {
      it("should take no elements from the stream") {
        val stream = Stream(1, 3, 4, 8)
        stream.takeWhile(_ < 0).toList shouldEqual List()
      }
      it("should take first element from the stream") {
        val stream = Stream(1, 3, 4, 8)
        stream.takeWhile(_ == 1).toList shouldEqual List(1)
      }
      it("should take first 2 elements from the stream") {
        val stream = Stream(1, 3, 4, 8)
        stream.takeWhile(_ % 2 == 1).toList shouldEqual List(1, 3)
      }
      it("should take all elements from the stream") {
        val stream = Stream(1, 3, 4, 8)
        stream.takeWhile(_ > 0).toList shouldEqual List(1, 3, 4, 8)
      }
    }
    describe("Exercise 5.4: forAll") {
      it("should return true") {
        val oddNumbers = Stream(1, 3, 5, 7)
        oddNumbers.forAll(_ % 2 == 1) shouldBe true
        val evenNumbers = Stream(0, 2, 4, 6, 8)
        evenNumbers.forAll(_ % 2 == 0) shouldBe true
      }
      it("should return false") {
        val stream = Stream(1, 2, 3, 5, 7)
        stream.forAll(_ % 2 == 1) shouldBe false
        val stream2 = Stream(0, 2, 4, 6, 7, 8)
        stream2.forAll(_ % 2 == 0) shouldBe false
      }
    }
    describe("Exercise 5.5: takeWhileFR") {
      it("should take no elements from the stream") {
        val stream = Stream(1, 3, 4, 8)
        stream.takeWhileFR(_ < 0).toList shouldEqual List()
      }
      it("should take first element from the stream") {
        val stream = Stream(1, 3, 4, 8)
        stream.takeWhileFR(_ == 1).toList shouldEqual List(1)
      }
      it("should take first 2 elements from the stream") {
        val stream = Stream(1, 3, 4, 8)
        stream.takeWhileFR(_ % 2 == 1).toList shouldEqual List(1, 3)
      }
      it("should take all elements from the stream") {
        val stream = Stream(1, 3, 4, 8)
        stream.takeWhileFR(_ > 0).toList shouldEqual List(1, 3, 4, 8)
      }
    }
    describe("Exercise 5.6: headOption") {
      it("should return Some of the first element") {
        val stream = Stream(1, 3, 4, 8)
        stream.headOption() shouldEqual Some(1)
      }
      it("should return None") {
        val stream = Stream()
        stream.headOption() shouldEqual None
      }
    }
    describe("Exercise 5.7: map") {
      it("should multiply each element by 2") {
        val stream = Stream(1, 3, 4, 8)
        stream.map(_ * 2).toList shouldEqual List(2, 6, 8, 16)
      }
      it("should return empty stream") {
        val stream = Stream[Int]()
        stream.map(_ * 2).toList shouldEqual List()
      }
    }
    describe("Exercise 5.7: append") {
      it("should return list of 6 elements") {
        val stream = Stream(1, 2, 3)
        val stream2 = Stream(7, 8, 9)
        stream.append(stream2).toList shouldEqual List(1, 2, 3, 7, 8, 9)
        stream2.append(stream).toList shouldEqual List(7, 8, 9, 1, 2, 3)
      }
      it("should return empty stream") {
        val stream = Stream(1, 2, 3)
        val stream2 = Stream()
        stream.append(stream2).toList shouldEqual stream.toList
        stream2.append(stream).toList shouldEqual stream.toList
      }
    }
    describe("Exercise 5.7: flatMap") {
      it("should multiply each element by 2") {
        val stream = Stream(1, 3, 4, 8)
        stream.flatMap(v => Stream(v * 2)).toList shouldEqual List(2, 6, 8, 16)
      }
      it("should return empty stream") {
        val stream = Stream[Int]()
        stream.flatMap(v => Stream(v * 2)).toList shouldEqual List()
      }
    }
    describe("Exercise 5.8: constant") {
      it("should return infinite stream of specified value") {
        val stream = Stream.constant(1)
        stream.take(10).toList shouldEqual List(1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
        stream.take(5).toList shouldEqual List(1, 1, 1, 1, 1)
        stream.take(2).toList shouldEqual List(1, 1)
        stream.take(0).toList shouldEqual List()
      }
    }
    describe("Exercise 5.9: from") {
      it("should return infinite stream integers starting from n and incremented") {
        val stream = Stream.from(1)
        stream.take(10).toList shouldEqual List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
        stream.take(5).toList shouldEqual List(1, 2, 3, 4, 5)
        stream.take(2).toList shouldEqual List(1, 2)
        stream.take(0).toList shouldEqual List()
      }
    }
    describe("Exercise 5.10: fibs") {
      it("should return infinite stream of Fibonacci numbers") {
        val stream = Stream.fibs()
        stream.take(10).toList shouldEqual List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)
        stream.take(7).toList shouldEqual List(0, 1, 1, 2, 3, 5, 8)
        stream.take(3).toList shouldEqual List(0, 1, 1)
        stream.take(2).toList shouldEqual List(0, 1)
        stream.take(1).toList shouldEqual List(0)
      }
    }
    describe("Exercise 5.11: unfold") {
      it("should return infinite stream integers starting from 10 and incremented") {
        val stream = Stream.unfold(1)(s => Some((s, s + 1)))
        stream.take(10).toList shouldEqual List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
        stream.take(5).toList shouldEqual List(1, 2, 3, 4, 5)
        stream.take(2).toList shouldEqual List(1, 2)
        stream.take(0).toList shouldEqual List()
      }
    }
  }
}
