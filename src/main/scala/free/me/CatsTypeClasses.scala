package free.me
import java.util.Date
object CatsTypeClasses {
  import cats._
  import cats.implicits._
  implicit val dateShow: Show[Date] = (t: Date) => s"${t.getTime}ms since epoch."
  println(new Date().show)

  // monoid
  Monoid[String].combine("Hello", "World")
}
