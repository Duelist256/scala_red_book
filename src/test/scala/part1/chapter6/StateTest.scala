package part1.chapter6

import org.scalatest.{FunSpec, Matchers}

class StateTest extends FunSpec with Matchers {
  describe("StateTest") {
    describe("Exercise 6.10: unit") {
      it("should return value passed to unit") {
        val initialValue = 10
        val initialState: State[String, Int] = State.unit(initialValue)
        val (newValue, _) = initialState.run("")
        newValue shouldEqual initialValue
      }
    }
    describe("Exercise 6.10: map") {
      it("should multiple integer by 10") {
        val initialValue = 10
        val initialState: State[String, Int] = State[String, Int](s => (initialValue, s))
        val (newValue, _) = initialState.map(_ * 10).run("")
        newValue shouldEqual 100
      }
    }
    describe("Exercise 6.10: flatMap") {
      it("should multiple integer by 10") {
        val initialValue = 10
        val initialState: State[String, Int] = State[String, Int](s => (initialValue, s))
        val (newValue, _) = initialState.flatMap(i => State.unit(i * 10)).run("")
        newValue shouldEqual 100
      }
    }
    describe("Exercise 6.10: map2") {
      it("should concatenate integer and string") {
        val value1 = 10
        val value2 = "part"
        val state1: State[Any, Int] = State[Any, Int](s => (value1, s))
        val state2: State[Any, String] = State[Any, String](s => (value2, s))
        val newState = state1.map2[String, String](state2)((i, s) => s"$s$i")
        val (newValue, _) = newState.run(new Object)
        newValue shouldEqual "part10"
      }
    }
    describe("Exercise 6.10: sequence") {
      it("should return list of integers") {
        import State.unit
        val list = List[State[Any, Int]](unit(1), unit(2), unit(3), unit(4), unit(5))
        val (resultList, _) = State.sequence(list).run(())
        resultList shouldEqual List(1, 2, 3, 4, 5)
      }
    }
  }
}
