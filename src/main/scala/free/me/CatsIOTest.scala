package free.me
import cats.effect._
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future}
object CatsIOTest extends App {
  override def run(args: List[String]): IO[ExitCode] = {
    IO(println("hello world"))
      .flatMap { _ =>
        implicit val ec: ExecutionContextExecutor = ExecutionContext.global
        Async.fromFuture(IO(Future {
          println("hello World")
        }))
      }.flatMap(_ => IO(0))
      .map(ExitCode(_))
  }
}
