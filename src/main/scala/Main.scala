import zio.{App, IO, ZIO}
import zio.console._

object Main extends App {
  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    putStrLn("Hello world!")
      .map(_ => 0)
}
