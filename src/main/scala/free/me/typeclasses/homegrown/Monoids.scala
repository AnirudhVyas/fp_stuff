package free.me.typeclasses.homegrown
import cats.kernel.Semigroup
/**
 * Lays out type classes that can combine and have an identity.
 */
object Monoids {
  /**
   * A type class (a class of types) that has a capability to combine
   * values of same types `A`
   *
   * @tparam A operand/combination operand type.
   */
  trait SemiGroup[A] {
    def combine(x: A, y: A): A
  }
  /**
   * A specialized semigroup that applies to `A` types with identity.
   *
   * @tparam A type parameter `A`
   */
  trait Monoid[A] extends Semigroup[A] {
    def identity(a: A): A
  }
  implicit object MonoidImplicit extends Monoid[Int] {
    override def combine(x: Int, y: Int): Int = x + y
    override def identity(a: Int): Int = a
  }
  implicit object MonoidOptionString extends Monoid[Option[String]] {
    override def identity(a: Option[String]): Option[String] = a
    override def combine(x: Option[String], y: Option[String]): Option[String] = (x, y) match {
      case (Some(a), Some(b)) => Option(s"$a:$b")
      case (None, Some(b)) => Option(s"$b")
      case (Some(a), None) => Option(s"$a")
      case (None, None) => None
    }
  }
  object Monoid {
    def apply[A: Monoid]: Monoid[A] = implicitly[Monoid[A]]
  }
}
