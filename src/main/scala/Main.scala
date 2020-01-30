import interoperabillity.{CatsIOInterop, ScalaFuturesInterop, TwitterFuturesInterop, ZioInterop}
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

        ScalaFuturesInterop.withFinalTagless()
        ScalaFuturesInterop.withCatsIO()
        ScalaFuturesInterop.withMonix()
        ScalaFuturesInterop.withTwitterFuture()
        ScalaFuturesInterop.withZio()

        CatsIOInterop.withFinalTagless()
        CatsIOInterop.withMonix()
        CatsIOInterop.withTwitterFuture()
        CatsIOInterop.withScalaFuture()
        CatsIOInterop.withZio()

        TwitterFuturesInterop.withFinalTagless()
        TwitterFuturesInterop.withCatsIO()
        TwitterFuturesInterop.withMonix()
        TwitterFuturesInterop.withScalaFuture()
        TwitterFuturesInterop.withZio()

        // todo cats FinalTagless

        0
      }

}
