import interoperabillity.{
  CatsFinalTaglessInterop,
  CatsIOInterop,
  MonixInterop,
  ScalaFuturesInterop,
  TwitterFuturesInterop,
  ZioInterop
}
import zio.console._
import zio._
import cats.effect.{IO => CatsIO}

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

        CatsFinalTaglessInterop.withCatsIO[CatsIO]()
        CatsFinalTaglessInterop.withMonix[CatsIO]()
        CatsFinalTaglessInterop.withTwitterFuture[CatsIO]()
        CatsFinalTaglessInterop.withScalaFuture[CatsIO]()
        CatsFinalTaglessInterop.withZio[CatsIO]()

        MonixInterop.withFinalTagless()
        MonixInterop.WithCatsIO()
        MonixInterop.withTwitterFuture()
        MonixInterop.withScalaFuture()
        MonixInterop.withZio()

        0
      }

}
