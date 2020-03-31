package free.me.typeclasses.cats
import cats.data.Writer
import cats.instances.vector._
import cats.syntax.applicative._
import cats.syntax.writer._
object WriterTestApp extends App {
  type Logged[A] = Writer[Vector[String], A]
  val (log, result) = 123.pure[Logged].run
  println(s"($log,$result)")
  val (log1, result1) = Vector("msg1", "msg2", "msg3").tell.run
  println(s"($log1,$result1)")
  val w = for {
    a <- 10.pure[Logged]
    _ <- Vector("a", "b", "c").tell
    b <- 32.writer(Vector("x", "y", "z"))
  } yield a + b
  val (l, res) = w.run
  println(s"==>$l,$res")
}
