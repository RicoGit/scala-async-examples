import interoperabillity.{CatsIOInterop, ScalaFutureInterop, ZioInterop}
import zio.console._
import zio._

object Main extends App {

  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    putStrLn("Start")
      .map { _ =>
        ZioInterop.withFinalTagless()
        ZioInterop.withCatsIO()
        ZioInterop.withMonix()
        ZioInterop.withTwitterFuture()
        ZioInterop.withScalaFuture()

        ScalaFutureInterop.withFinalTagless()
        ScalaFutureInterop.withCatsIO()
        ScalaFutureInterop.withMonix()
        ScalaFutureInterop.withTwitterFuture()
        ScalaFutureInterop.withZio()

        CatsIOInterop.withFinalTagless()
        CatsIOInterop.withMonix()
        CatsIOInterop.withTwitterFuture()
        CatsIOInterop.withScalaFuture()
        CatsIOInterop.withZio()

        0
      }

}
