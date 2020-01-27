import interoperabillity.ZioInterop
import zio.console._
import zio._

object Main extends App {

  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    putStrLn("Hello world!")
      .map { _ =>
        ZioInterop.withFinalTagless()
        ZioInterop.withCatsIO()
        ZioInterop.withMonix()
        ZioInterop.withTwitterFuture()
        ZioInterop.withScalaFuture()
        0
      }

}
