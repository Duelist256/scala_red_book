package part1.chapter3

import part1.chapter3.List._
import org.scalatest.{FunSpec, FunSuite, Matchers}

class ListTest extends FunSpec with Matchers {
  describe("List test") {
    describe("Exercise 3.2: tail") {
      it("should return nonempty tail of list or Nil if list is empty") {
        val list = List(1, 2, 3)
        tail(list) shouldEqual List(2, 3)
        tail(tail(list)) shouldEqual List(3)
        tail(tail(tail(list))) shouldEqual Nil
        tail(tail(tail(tail(list)))) shouldEqual Nil
      }
    }
    describe("Exercise 3.3: setHead") {
      it("should return list with new head") {
        val list = List(1, 2, 3)
        setHead(5, list) shouldEqual List(5, 2, 3)
        setHead(5, tail(list))  shouldEqual List(5, 3)
        setHead(5, Nil) shouldEqual List(5)
      }
    }
    describe("Exercise 3.4: drop") {
      it("should return nonempty list or Nil if list is empty") {
        val list = List(1, 2, 3, 4, 5)
        drop(list, 1) shouldEqual List(2, 3, 4, 5)
        drop(list, 2) shouldEqual List(3, 4, 5)
        drop(list, 3) shouldEqual List(4, 5)
        drop(list, 4) shouldEqual List(5)
        drop(Nil, 4) shouldEqual Nil
      }
    }
     describe("Exercise 3.5: dropWhile") {
       it("should return nonempty list or Nil if list is empty") {
         val list = List(1, 3, 5, 7, 8)
         dropWhile[Int](list, _ <= 3) shouldEqual List(5, 7, 8)
         dropWhile[Int](list, _ % 2 == 1) shouldEqual List(8)
         dropWhile[Int](list, _ > 0) shouldEqual Nil
         dropWhile[Int](Nil, _ < 0) shouldEqual Nil
       }
     }
     describe("Exercise 3.6: init") {
       it("should return nonempty list or Nil if list is empty") {
         val list = List(1, 2, 3, 4, 5)
         init(list) shouldEqual List(1, 2, 3, 4)
       }
     }
     describe("Exercise 3.8: foldRight") {
       it("should return nonempty result") {
         val list = List(1, 2, 3, 4)
         foldRight(list, List[Int]())(Cons(_, _)) shouldEqual List(1, 2, 3, 4)
         foldRight(list, 0)(_ + _) shouldEqual 10
         foldRight(list, 1)(_ * _) shouldEqual 24
         foldRight(List[Int](), 1)(_ * _) shouldEqual 1
       }
     }
     describe("Exercise 3.9: setHead") {
       it("should return length of list") {
         List.length(List(1, 2, 3)) shouldEqual 3
         List.length(List(1, 2, 3, 4, 5)) shouldEqual 5
         List.length(List()) shouldEqual 0
       }
     }
     describe("Exercise 3.10: foldLeft") {
       it("should return nonempty result") {
         val list = List(1, 2, 3, 4, 5)
         foldLeft(list, 0)(_ + _) shouldEqual 15
         foldLeft(list, 1)(_ * _) shouldEqual 120

         foldLeft(List(1, 2, 3), 1)(_ * _) shouldEqual 6
         foldLeft(List(3), 1)(_ + _) shouldEqual 4

         foldLeft(List[Int](), 1)(_ + _) shouldEqual 1
         foldLeft(List[Int](), 1)(_ * _) shouldEqual 1
       }
     }
     describe("Exercise 3.11: sumFl, productFl, lengthFl") {
       val ints = List(1, 2, 3, 4, 5)
       val ds = List(2, 1.5, 3)
       val emptyI = List[Int]()
       val emptyD = List[Double]()
       it("should return sum of list") {
         sumFl(ints) shouldEqual 15
         sumFl(emptyI) shouldEqual 0
       }
       it("should return product of list") {
         productFl(ds) shouldEqual 9.0
         productFl(emptyD) shouldEqual 1.0
       }
       it("should return length of list") {
         lengthFl(ints) shouldEqual 5
         lengthFl(ds) shouldEqual 3
         lengthFl(emptyI) shouldEqual 0
         lengthFl(emptyD) shouldEqual 0
       }
     }
     describe("Exercise 3.12: reverse") {
       it("should return nonempty list or Nil if list is empty") {
         val list = List(1, 2, 3, 4, 5)
         reverse(list) shouldEqual List(5, 4, 3, 2, 1)
         reverse(Nil) shouldEqual Nil
       }
     }
     describe("Exercise 3.13: foldLeftFR, foldRightFL") {
       it("should return nonempty result") {
         val list = List(1, 2, 3, 4, 5)
         val f: (Int, Int) => Int = _ - _
         foldLeftFR(list, 0)(f) shouldEqual foldLeft(list, 0)(f)
         foldRight(list, 0)(f) shouldEqual foldRightFL(list, 0)(f)
       }
     }
     describe("Exercise 3.14: appendFR") {
       it("should append two lists") {
         val list1 = List(1, 2, 3, 4)
         val list2 = List(5, 6, 7)
         appendFR(list1, list2) shouldEqual List(1, 2, 3, 4, 5, 6, 7)
       }
     }
     describe("Exercise 3.15: flatten") {
       it("should convert list of lists to one list") {
         val listOfLists = List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))
         flatten(listOfLists) shouldEqual List(1, 2, 3, 4, 5, 6, 7, 8, 9)
       }
     }
     describe("Exercise 3.16: incEachElemOfList") {
       it("should return nonempty list of incremented elements or Nil if list is empty") {
         val list = List(1, 2, 3, 4)
         incEachElemOfList(list) shouldEqual List(2, 3, 4, 5)
       }
     }
     describe("Exercise 3.17: turnDoubleIntoString") {
       it("should return nonempty list with converted elements or Nil if list is empty") {
         val list = List(2.7, 11.11, 2.28, 0.0)
         turnDoubleIntoString(list) shouldEqual List("2.7", "11.11", "2.28", "0.0")
         turnDoubleIntoString(List[Double]()) shouldEqual Nil
       }
     }
     describe("Exercise 3.18: map") {
       it("should return nonempty list with converted elements or Nil if list is empty") {
         val list = List(2.7, 11.11, 2.28, 0.0)
         val list2 = List("Hello", "World", "Whatcha", "doin")
         map(list)(_ * 2.0) shouldEqual List(5.4, 22.22, 4.56, 0.0)
         map(list2)(_.reverse) shouldEqual List("olleH", "dlroW", "ahctahW", "niod")
         map(List[String]())(_.reverse) shouldEqual Nil
       }
     }
     describe("Exercise 3.19: filter") {
       it("should return nonempty filtered list or Nil if list is empty") {
         val list = List(1, -1, -2, 2, 3, 4, -7, 5)
         filter(list)(_ > 0) shouldEqual List(1, 2, 3, 4, 5)
         val list2 = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
         filter(list2)(_ % 2 == 1) shouldEqual List(1, 3, 5, 7, 9)
         filter(List[Int]())(_ % 2 == 1) shouldEqual Nil
       }
     }
     describe("Exercise 3.20: flatMap") {
       it("should return nonempty list or Nil if list is empty") {
         val list = List(2, 3, 4, 5)
         flatMap(list)(e => List(e, e * e)) shouldEqual List(2, 4, 3, 9, 4, 16, 5, 25)
         flatMap(List[Int]())(e => List(e, e * e)) shouldEqual Nil
       }
     }
    describe("Exercise 3.21: filterViaFM") {
      it("should return nonempty filtered list or Nil if list is empty") {
        val list = List(1, -1, -2, 2, 3, 4, -7, 5)
        filterViaFM(list)(_ > 0) shouldEqual List(1, 2, 3, 4, 5)
        val list2 = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
        filterViaFM(list2)(_ % 2 == 1) shouldEqual List(1, 3, 5, 7, 9)
        filterViaFM(List[Int]())(_ % 2 == 1) shouldEqual Nil
      }
    }
    describe("Exercise 3.22: sumEachElem") {
      it("should return new list which contains sums of elements or Nil if one of the lists is Nil") {
        val list1 = List(1, 2, 3, 4, 5)
        val list2 = List(2, 3, 4, 5, 6, 3)
        sumEachElem(list1, list2) shouldEqual List(3, 5, 7, 9, 11)
        val list3 = List(1, 2, 3, 4, 5, 7)
        val list4 = List(1)
        sumEachElem(list3, list4) shouldEqual List(2)
        sumEachElem(list3, Nil) shouldEqual Nil
      }
    }
    describe("Exercise 3.23: zipWith") {
      it("should zip two lists or Nil if one of the lists is Nil") {
        val list1 = List(1, 2, 3, 4, 5)
        val list2 = List(2, 3, 4, 5, 6, 3)
        zipWith(list1, list2)(_ + _) shouldEqual List(3, 5, 7, 9, 11)
        val list3 = List("Ba", "Or", "Pine", "Ap", "Water", "Nil")
        val list4 = List("nana", "ange", "apple", "ple", "melon")
        zipWith(list3, list4)(_ + _) shouldEqual List("Banana", "Orange", "Pineapple", "Apple", "Watermelon")
        zipWith(list3, Nil)(_ + _) shouldEqual Nil
      }
    }
    describe("Exercise 3.24: hasSubsequence") {
      it("should return true if list has subsequence or not if it doesn't") {
        val list = List(1, 2, 3, 4, 5)
        val subNil = Nil
        val sub1 = List(1)
        val sub2 = List(2)
        val sub3 = List(3)
        val sub4 = List(4)
        val sub5 = List(5)
        val sub12 = List(1, 2)
        val sub23 = List(2, 3)
        val sub34 = List(3, 4)
        val sub45 = List(4, 5)
        val sub123 = List(1, 2, 3)
        val sub234 = List(2, 3, 4)
        val sub345 = List(3, 4, 5)
        hasSubsequence(list, subNil) shouldBe true
        hasSubsequence(list, sub1) shouldBe true
        hasSubsequence(list, sub2) shouldBe true
        hasSubsequence(list, sub3) shouldBe true
        hasSubsequence(list, sub4) shouldBe true
        hasSubsequence(list, sub5) shouldBe true
        hasSubsequence(list, sub12) shouldBe true
        hasSubsequence(list, sub23) shouldBe true
        hasSubsequence(list, sub34) shouldBe true
        hasSubsequence(list, sub45) shouldBe true
        hasSubsequence(list, sub123) shouldBe true
        hasSubsequence(list, sub234) shouldBe true
        hasSubsequence(list, sub345) shouldBe true
        val sub6 = List(6)
        val sub21 = List(2, 1)
        val sub32 = List(3, 2)
        val sub35 = List(3, 5)
        val sub51 = List(5, 1)
        val sub43 = List(4, 3)
        val sub413 = List(4, 1, 3)
        hasSubsequence(list, sub6) shouldBe false
        hasSubsequence(list, sub21) shouldBe false
        hasSubsequence(list, sub32) shouldBe false
        hasSubsequence(list, sub35) shouldBe false
        hasSubsequence(list, sub51) shouldBe false
        hasSubsequence(list, sub43) shouldBe false
        hasSubsequence(list, sub413) shouldBe false
      }
    }
  }
}
