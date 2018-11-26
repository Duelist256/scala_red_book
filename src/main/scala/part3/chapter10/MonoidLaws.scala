package part3.chapter10

import part2.chapter8.{Gen, Prop}

object MonoidLaws {
  import Prop.{forAll}

  /* Exercise 10.4
     Use the property-based testing framework we developed in part 2 to implement a
     property for the monoid laws. Use your property to test the monoids we’ve written. */
  def associativity[A](m: Monoid[A])(in: Gen[A]): Prop = Prop.forAll(
    // for comprehension doesn't work :(
    in.flatMap(x => in.flatMap(y => in.flatMap(z => Gen.unit((x, y, z)))))
  ) { case (x, y, z) => m.op(m.op(x, y), z) == m.op(x, m.op(y, z)) }

  def identity[A](m: Monoid[A])(in: Gen[A]): Prop = forAll(in)(v => m.op(v, m.zero) == m.op(m.zero, v))

  def monoidLaws[A](m: Monoid[A])(in: Gen[A]): Prop = {
    associativity(m)(in) && identity(m)(in)
  }

  def main(args: Array[String]): Unit = {
    Prop.run(monoidLaws(MonoidInstances.intAddition)(Gen.int))
    Prop.run(monoidLaws(MonoidInstances.intMultiplication)(Gen.int))
    Prop.run(monoidLaws(MonoidInstances.booleanOr)(Gen.boolean))
    Prop.run(monoidLaws(MonoidInstances.booleanAnd)(Gen.boolean))
    Prop.run(monoidLaws(MonoidInstances.optionMonoid[String])(Gen.unit(Some("foo"))))
    Prop.run(monoidLaws(MonoidInstances.optionMonoid[String])(Gen.unit(None)))
    Prop.run(monoidLaws(MonoidInstances.wcMonoid)(Gen.int.flatMap(i => Gen.unit(Stub(i.toString)))))
    Prop.run(monoidLaws(MonoidInstances.wcMonoid)(generatePart()))
    // is it possible to compare lambdas?
    // Prop.run(identity(MonoidInstances.endoMonoid[String])(Gen.unit(s => s.concat(s))))
  }

  private def generatePart(): Gen[WC] = {
    val left = Gen.int.flatMap(i => Gen.unit(i.toString))
    val words = Gen.int
    val right = Gen.int.flatMap(i => Gen.unit(i.toString))
    left.flatMap(l => words.flatMap(w => right.flatMap(r => Gen.unit(Part(l, w, r)))))
  }
}