package free.me.typeclasses.cats
import cats.effect._
/**
 * IO Monad from cats and Cats effect stuff.
 * TODO: Add more descriptions of operations and also show unsafe run sync/async operations.
 */
object CatsIOVanillaExample extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    val jobOne = IO {
      println("job one running")
      Thread.sleep(5000)
      println("job one completed")
    }
    val jobTwo = IO {
      println("job two running")
      Thread.sleep(5000)
      println("job two completed")
    }
    val x: IO[(Unit, Unit)] = for {
      j1 <- jobOne
      j2 <- jobTwo
    } yield (j1, j2)
    println("== descriptive program now has to run ==")
    x.unsafeRunSync()
    IO(ExitCode.Success)
  }
}
