package part1.chapter8

object Properties {

  val smallInt: Gen[Int] = Gen.choose(-10,10)
  val maxProp: Prop = Prop.forAll(Gen.listOf1(smallInt)) { ns =>
    val max = ns.max
    !ns.exists(_ > max)
  }

  /* Exercise 8.14
     Write a property to verify the behavior of List.sorted (API docs link: http://mng.bz/Pz86),
     which you can use to sort (among other things) a List[Int]
     For instance, List(2,1,3).sorted is equal to List(1,2,3). */

  val range: Gen[Int] = Gen.choose(2, 10)
  val sorted: Prop = Prop.forAll(Gen.listOf1(range)) { ns =>
    val list = ns.sorted
    val sub = list.tail

    list.zip(sub).forall {
      case (a, b) => a <= b
    }
  }

  def main(args: Array[String]): Unit = {
    Prop.run(sorted, 3, 2)
  }
}