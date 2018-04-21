package chapter2.part3

object Exercise3_8 extends App {
  import List.foldRight

  println(foldRight(List(1,2,3), Nil:List[Int])(Cons(_, _)))
}