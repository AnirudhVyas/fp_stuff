package free.me.typeclasses.homegrown.typeclasses.homegrown

/**
 * Functional and recursive datastructures (e.g. Cons)
 */
object FunctionalDataStructures {
  sealed trait VList[+A]
  case object Nil extends VList[Nothing]
  case class Cons[+A](h: A, tail: VList[A]) extends VList[A]
  val x: Cons[Int] = Cons[Int](1, Nil)
  val y: Cons[Int] = Cons[Int](2, x) // composing stuff yay!
  val x1: List[Set[Option[String]]] =
    List.empty[String]
      .map(e => Option(e))
      .map(e => Set(e)) // map applies a function that takes pure values
  // and return pure values which are then boxed within the type for which this functor typeclass applies to
  // however flatmap
  val x2: List[Option[String]] =
  List.empty[String]
    .flatMap(e => Some(s"$e:2"))
    .map(e => Option(e))
}
