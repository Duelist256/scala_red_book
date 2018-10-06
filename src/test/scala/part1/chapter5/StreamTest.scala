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
    describe("Exercise 5.12: fibsU") {
      it("should return infinite stream of specified value") {
        val stream = Stream.fibsU()
        stream.take(10).toList shouldEqual List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)
        stream.take(7).toList shouldEqual List(0, 1, 1, 2, 3, 5, 8)
        stream.take(3).toList shouldEqual List(0, 1, 1)
        stream.take(2).toList shouldEqual List(0, 1)
        stream.take(1).toList shouldEqual List(0)
      }
    }
    describe("Exercise 5.12: fromU") {
      it("should return infinite stream of specified value") {
        val stream = Stream.fromU(1)
        stream.take(10).toList shouldEqual List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
        stream.take(5).toList shouldEqual List(1, 2, 3, 4, 5)
        stream.take(2).toList shouldEqual List(1, 2)
        stream.take(0).toList shouldEqual List()
      }
    }
    describe("Exercise 5.12: constantU") {
      it("should return infinite stream of specified value") {
        val stream = Stream.constantU(1)
        stream.take(10).toList shouldEqual List(1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
        stream.take(5).toList shouldEqual List(1, 1, 1, 1, 1)
        stream.take(2).toList shouldEqual List(1, 1)
        stream.take(0).toList shouldEqual List()
      }
    }
    describe("Exercise 5.12: onesU") {
      it("should return infinite stream of specified value") {
        val stream = Stream.onesU
        stream.take(10).toList shouldEqual List(1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
        stream.take(5).toList shouldEqual List(1, 1, 1, 1, 1)
        stream.take(2).toList shouldEqual List(1, 1)
        stream.take(0).toList shouldEqual List()
      }
    }
    describe("Exercise 5.13: mapU") {
      it("should multiply each element by 2") {
        val stream = Stream(1, 3, 4, 8)
        stream.map(_ * 2).toList shouldEqual List(2, 6, 8, 16)
      }
      it("""should concatenate each element with "part"""") {
        val stream = Stream(1, 3, 4, 8)
        stream.map(n => s"part$n").toList shouldEqual List("part1", "part3", "part4", "part8")
      }
      it("should return empty stream") {
        val stream = Stream[Int]()
        stream.map(_ * 2).toList shouldEqual List()
      }
    }
    describe("Exercise 5.13: takeU") {
      it("should take no elements from the stream") {
        val stream = Stream(1, 2, 3, 4, 5)
        stream.takeU(-1).toList shouldEqual List()
      }
      it("should take no elements from the stream 2") {
        val stream = Stream(1, 2, 3, 4, 5)
        stream.takeU(0).toList shouldEqual List()
      }
      it("should take first element from the stream") {
        val stream = Stream(1, 2, 3, 4, 5)
        stream.takeU(1).toList shouldEqual List(1)
      }
      it("should take first 3 elements from the stream") {
        val stream = Stream(1, 2, 3, 4, 5)
        stream.takeU(3).toList shouldEqual List(1, 2, 3)
      }
      it("should take first 5 elements from the stream") {
        val stream = Stream(1, 2, 3, 4, 5)
        stream.takeU(5).toList shouldEqual List(1, 2, 3, 4, 5)
      }
      it("should take all elements from the stream") {
        val stream = Stream(1, 2, 3, 4, 5)
        stream.takeU(55).toList shouldEqual List(1, 2, 3, 4, 5)
      }
    }
    describe("Exercise 5.13: takeWhileU") {
      it("should take no elements from the stream") {
        val stream = Stream(1, 3, 4, 8)
        stream.takeWhileU(_ < 0).toList shouldEqual List()
      }
      it("should take first element from the stream") {
        val stream = Stream(1, 3, 4, 8)
        stream.takeWhileU(_ == 1).toList shouldEqual List(1)
      }
      it("should take first 2 elements from the stream") {
        val stream = Stream(1, 3, 4, 8)
        stream.takeWhileU(_ % 2 == 1).toList shouldEqual List(1, 3)
      }
      it("should take all elements from the stream") {
        val stream = Stream(1, 3, 4, 8)
        stream.takeWhileU(_ > 0).toList shouldEqual List(1, 3, 4, 8)
      }
    }
    describe("Exercise 5.13: zipWith") {
      it("should sum each element of the streams") {
        val stream = Stream(1, 2, 3, 4)
        val stream2 = Stream(5, 6, 7, 8)
        stream.zipWith(stream2)(_ + _).toList shouldEqual List(6, 8, 10, 12)
        stream.takeU(3).zipWith(stream2)(_ + _).toList shouldEqual List(6, 8, 10)
        stream.zipWith(stream2.takeU(3))(_ + _).toList shouldEqual List(6, 8, 10)
        stream.takeU(2).zipWith(stream2)(_ + _).toList shouldEqual List(6, 8)
        stream.zipWith(stream2.takeU(2))(_ + _).toList shouldEqual List(6, 8)
        stream.takeU(1).zipWith(stream2)(_ + _).toList shouldEqual List(6)
        stream.zipWith(stream2.takeU(1))(_ + _).toList shouldEqual List(6)
        stream.take(0).zipWith(stream2)(_ + _).toList shouldEqual List()
        stream.zipWith(stream2.take(0))(_ + _).toList shouldEqual List()
      }
    }
    describe("Exercise 5.13: zipAll") {
      it("should zip each element of the streams") {
        val stream = Stream(1, 2, 3, 4)
        val stream2 = Stream(5, 6, 7, 8)
        stream.zipAll(stream2).toList shouldEqual List((Some(1), Some(5)), (Some(2), Some(6)), (Some(3), Some(7)), (Some(4), Some(8)))
        stream.takeU(3).zipAll(stream2).toList shouldEqual List((Some(1), Some(5)), (Some(2), Some(6)), (Some(3), Some(7)), (None, Some(8)))
        stream.zipAll(stream2.takeU(3)).toList shouldEqual List((Some(1), Some(5)), (Some(2), Some(6)), (Some(3), Some(7)), (Some(4), None))
        stream.takeU(2).zipAll(stream2).toList shouldEqual List((Some(1), Some(5)), (Some(2), Some(6)), (None, Some(7)), (None, Some(8)))
        stream.zipAll(stream2.takeU(2)).toList shouldEqual List((Some(1), Some(5)), (Some(2), Some(6)), (Some(3), None), (Some(4), None))
        stream.takeU(1).zipAll(stream2).toList shouldEqual List((Some(1), Some(5)), (None, Some(6)), (None, Some(7)), (None, Some(8)))
        stream.zipAll(stream2.takeU(1)).toList shouldEqual List((Some(1), Some(5)), (Some(2), None), (Some(3), None), (Some(4), None))
        stream.takeU(0).zipAll(stream2).toList shouldEqual List((None, Some(5)), (None, Some(6)), (None, Some(7)), (None, Some(8)))
        stream.zipAll(stream2.takeU(0)).toList shouldEqual List((Some(1), None), (Some(2), None), (Some(3), None), (Some(4), None))
      }
    }
  }
}
