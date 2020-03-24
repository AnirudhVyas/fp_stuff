package free.me
import cats.effect._
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future}
/**
 * IO Monad from cats and Cats effect stuff.
 * TODO: Add more descriptions of operations and also show unsafe run sync/async operations.
 */
object CatsIOTest extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    IO(println("hello world")).map(e => e).flatMap { e =>
      implicit val ec: ExecutionContextExecutor = ExecutionContext.global
      Async.fromFuture(IO(Future {
        println("hello World")
      }))
    }.flatMap(_ => IO(0))
      .map(ExitCode(_))
  }
}
