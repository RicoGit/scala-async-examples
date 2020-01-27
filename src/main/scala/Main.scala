import Main.unsafeRun
import cats.effect.{Async, ConcurrentEffect, IO => CatsIO}
import com.twitter.util.{Future                 => TFuture}
import monix.eval.{Task                         => MonixTask}
import zio.console._
import zio._

import scala.concurrent.{ExecutionContext, Future}
import scala.language.higherKinds

object cases {

  // tagless final with cats effect
  object taglessFinal {

    def doAsync[F[_]: Async, T](t: T): F[T] = Async[F].pure(t)

    def doAsyncEither[F[_]: Async, E, T](t: T): F[Either[E, T]] =
      Async[F].pure(Right(t))

    def doEffect[F[_]: ConcurrentEffect, E, T](t: T): F[Either[E, T]] =
      Async[F].pure(Right(t))

  }

  // IO from cats effect
  object catsIO {

    def doIo[T](t: T): CatsIO[T] = CatsIO.pure(t)

    def doIoEither[E, T](t: T): CatsIO[Either[E, T]] = CatsIO.pure(Right(t))
  }

  // Monix

  def doMonix[T](t: T): MonixTask[T] = MonixTask(t)

  def doMonixEither[E, T](t: T): MonixTask[Either[E, T]] = MonixTask(Right(t))

  //  twitter future

  def doTFuture[E, T](t: T): TFuture[Either[E, T]] = TFuture(Right(t))

  def doTFutureEither[T](t: T): TFuture[T] = TFuture.value(t)

  // scala future

  def doFuture[E, T](t: T)(implicit ec: ExecutionContext): Future[Either[E, T]] = Future(Right(t))

  def doFutureEither[T](t: T): Future[T] = Future.successful(t)

  // ZIO

  def doZio[E, T](t: T): IO[E, T] = IO.fromEither(Right(t))

}

object Main extends App {

  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    putStrLn("Hello world!")
      .map { _ =>
        ZioInterop.withFinalTagless()
        ZioInterop.withCatsIO()
        0
      }

}

/** Interop between ZIO and other stuff each call should be mapped to IO[E, T] **/
object ZioInterop {
  import zio.interop.catz._

  def withFinalTagless(): Unit = {
    println("ZIO with cats tagless final: ")
    val res1: Task[String] = cases.taglessFinal.doAsync[Task, String]("async")
    val res11: IO[Int, String] = res1.mapError(_.getMessage.length)
    show(res11)

    val res2: Task[Either[Int, String]] =
      cases.taglessFinal.doAsyncEither[Task, Int, String]("async either")
    val res22: IO[Int, String] = res2.mapError(_.getMessage.length).absolve
    show(res22)

    {
      implicit val runtime: DefaultRuntime = new DefaultRuntime {}
      val res3: Task[Either[Int, String]] =
        cases.taglessFinal.doEffect[Task, Int, String]("effect")
      val res33: IO[Int, String] = res3.mapError(_.getMessage.length).absolve
      show(res33)
    }
  }

  def withCatsIO(): Unit = {
    println("ZIO with cats effect IO: ")

    val res1: Task[String] = cases.catsIO.doIo[String]("cats io").to[Task]
    val res11: IO[Int, String] = res1.mapError(_.getMessage.length)
    show(res11)

    val res2: Task[Either[Int, String]] = cases.catsIO.doIoEither[Int, String]("cats io either").to[Task]
    val res22: IO[Int, String] = res2.mapError(_.getMessage.length).absolve
    show(res22)
  }

  def show[E, T](io: IO[E, T]): Unit = println(" - " + unsafeRun(io))
}
