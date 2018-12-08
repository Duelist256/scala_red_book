package part3.chapter12

import part3.chapter11.Functor

trait Traverse[F[_]] extends Functor[F] {
  def traverse[G[_], A, B](fa: F[A])(f: A => G[B])(implicit G: Applicative[G]): G[F[B]]
  def sequence[G[_], A](fga: F[G[A]])(implicit G: Applicative[G]): G[F[A]] =
    traverse(fga)(identity)

  type Id[A] = A

  implicit val idApplicative = new Applicative[Id] {
    override def map2[A, B, C](a: Id[A], b: Id[B])(f: (A, B) => C): Id[C] = f(a, b)
    override def unit[A](a: => A): Id[A] = a
  }

  override def map[A, B](fa: F[A])(f: A => B): F[B] = traverse[Id, A, B](fa)(f)
}

object Traverse {
  /* Exercise 12.13
     Write Traverse instances for List, Option, and Tree. */
  case class Tree[+A](head: A, tail: List[Tree[A]])

  val listTraverse = new Traverse[List] {
    override def traverse[G[_], A, B](listA: List[A])(f: A => G[B])(implicit G: Applicative[G]): G[List[B]] =
      listA.foldRight(G.unit(List[B]()))((a, glistb) => G.map2(f(a), glistb)(_ :: _))
  }

  val optionTraverse = new Traverse[Option] {
    override def traverse[G[_], A, B](optA: Option[A])(f: A => G[B])(implicit G: Applicative[G]): G[Option[B]] =
      optA match {
        case None => G.unit(None)
        case Some(a) => G.map(f(a))(Some(_))
      }
  }

  val treeTraverse = new Traverse[Tree] {
    override def traverse[G[_], A, B](treeA: Tree[A])(f: A => G[B])(implicit G: Applicative[G]): G[Tree[B]] = treeA match {
      case Tree(h, list) =>
        G.map2(f(h), listTraverse.traverse(list)(a => traverse(a)(f)))(Tree(_, _))
    }
  }
}
