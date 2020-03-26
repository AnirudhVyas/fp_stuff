package free.me.typeclasses.cats
import cats.effect.IO
import retry._
object CatsRetry extends App {
  import cats.effect.Timer
  import scala.concurrent.ExecutionContext.global
  implicit val timer: Timer[IO] = IO.timer(global)
  val retryFiveTimes: RetryPolicy[IO] = RetryPolicies.limitRetries[IO](5)
  retryingOnSomeErrors[String](
    policy = retryFiveTimes,
    isWorthRetrying = (e: Throwable) => e.isInstanceOf[Throwable],
    onError = (error: Throwable, retryDetails) => {
      IO(println(s"retrying , retry details were: $retryDetails"))
    })(IO {
    throw new Exception("yo failure baby")
    "helloWorld"
  }).unsafeRunSync()
}
