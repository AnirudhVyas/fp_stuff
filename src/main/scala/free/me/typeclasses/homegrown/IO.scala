package free.me.typeclasses.homegrown
import scala.language.higherKinds
case class IOMonadException(msg: String, cause: Throwable) extends Throwable
/**
 * Homegrown IO Monad typeclass.
 *
 * @tparam A a type this monad isolates, typically a result of a side effecting operation.
 */
trait IO[A] {
  self =>
  def unsafeRun: A
  def flatMap[B](fa: IO[A])(f: A => IO[B]): IO[B] = new IO[B] {
    override def unsafeRun: B = f(fa.unsafeRun).unsafeRun
  }
  def map[B](xs: IO[A])(f: A => B): IO[B] = new IO[B] {
    override def unsafeRun: B = f(xs.unsafeRun)
  }
}
object IO {
  def apply[A](body: => A): IO[A] = new IO[A] {
    override def unsafeRun: A = body
    override def flatMap[B](fa: IO[A])(f: A => IO[B]): IO[B] = new IO[B] {
      override def unsafeRun: B = f(fa.unsafeRun).unsafeRun
    }
    override def map[B](xs: IO[A])(f: A => B): IO[B] = new IO[B] {
      override def unsafeRun: B = f(xs.unsafeRun)
    }
  }
}
object IOMonadTest extends App {
  // weird example, needs refinement and IO type also needs to be fleshed out more
  val unsafeOperation: IO[Unit] =
    IO(println("hello")).map[Unit](IO("world"))(x => println(s"$x bye bye"))
  println(unsafeOperation) // prints nothing ... nothing has happened ...
  println(unsafeOperation.unsafeRun) // now it ends the dream.
}