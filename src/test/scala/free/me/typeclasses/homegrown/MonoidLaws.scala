package free.me.typeclasses.homegrown
import org.scalacheck.{Gen, Prop, Properties}
/**
 * Monoid Laws are:
 * 1. identity
 * {{{
 *   Monoid[Option[String]].identity(optString) == optString
 * }}}
 * 2. associative
 * {{{
 *     val combineXy = Monoids.Monoid[Option[String]].combine(x, y)
 *     val combineYz = Monoids.Monoid[Option[String]].combine(y, z)
 *     Monoids.Monoid[Option[String]].combine(combineXy, z) == Monoids.Monoid[Option[String]].combine(x, combineYz)
 * }}}
 */
object MonoidLaws extends Properties("MonoidLaws") {
  val gen: Gen[Option[String]] = Gen.option[String](Gen.alphaUpperStr)
  val lowerAlphaStrGen: Gen[Option[String]] = Gen.option[String](Gen.alphaLowerStr)
  val alphaStrGen: Gen[Option[String]] = Gen.option[String](Gen.alphaStr)
  property("identity") = Prop.forAll(gen) { optString: Option[String] =>
    Monoids.Monoid[Option[String]].identity(optString) == optString
  }
  property("associative") = Prop.forAll(gen, lowerAlphaStrGen, alphaStrGen) { (x: Option[String], y: Option[String], z: Option[String]) =>
    val combineXy = Monoids.Monoid[Option[String]].combine(x, y)
    val combineYz = Monoids.Monoid[Option[String]].combine(y, z)
    Monoids.Monoid[Option[String]].combine(combineXy, z) == Monoids.Monoid[Option[String]].combine(x, combineYz)
  }
}
