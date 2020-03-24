package free.me
import free.me.FunctorLaws.FunctorSeeds.OptionFunctor
import free.me.HomeGrownTypeClasses.Functor
import org.scalacheck.{Prop, Properties}
/**
 * A Functor obeys two laws -
 * 1. composition
 * {{{
 * fe.map(f).map(g) == fs.map(f.andThen(g))
 * }}}
 * 2. identity
 * {{{
 *   fa.map(x => x) = fa
 * }}}
 * Sample functor tests.
 */
object FunctorLaws extends Properties("FunctorLaws") {
  object FunctorSeeds {
    implicit object OptionFunctor extends Functor[Option] {
      override def map[A, B](xs: Option[A])(f: A => B): Option[B] = xs.map(f)
    }
  }
  // sample functions for composition testing.
  val f: String => String = (e: String) => {
    s"$e:f"
  }
  val g: String => String = (e: String) => {
    s"$e:g"
  }
  property("composition") = Prop.forAll { option1: Option[String] =>
    val r1 = OptionFunctor.map[String, String](option1)(f)
    val r2 = OptionFunctor.map[String, String](r1)(g)
    val r3 = OptionFunctor.map[String, String](option1)(f.andThen(g))
    r2 == r3
  }
  property("identity") = Prop.forAll { option1: Option[String] =>
    OptionFunctor.map(option1)(identity[String]) == option1
  }
}
