package interoperabillity

import cats.effect.{Async, ContextShift, IO => CatsIO}
import cats.effect.implicits._
import cats.implicits._
import zio.{BootstrapRuntime, Task}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

/** Interop between Cats Final Tagless and other stuff. **/
object CatsFinalTaglessInterop {

  def withCatsIO[F[_]: Async](): Unit = {
    println("Cats FinalTagless with cats IO: ")

    val res1: CatsIO[String] = Cases.catsIO.doIo[String]("cats io")
    val res11: F[Either[Int, String]] = res1.attempt.map(_.left.map(_.getMessage.length)).to[F]
    show(res11)

    val res2: F[Either[Int, String]] = Cases.catsIO.doIoEither[Int, String]("async either").to[F]
    show(res2)

  }

  def withMonix[F[_]: Async](): Unit = {
    println("Cats FinalTagless with Monix: ")

    import monix.execution.Scheduler.Implicits.global

    val res1: F[String] = Cases.monix.doMonix[String]("monix task").to[F]
    val res11 = res1.attempt.map(_.left.map(_.getMessage.length))
    show(res11)

    val res2: F[Either[Int, String]] =
      Cases.monix.doMonixEither[Int, String]("monix task either").to[F]
    show(res2)

  }

  def withTwitterFuture[F[_]: Async](): Unit = {
    println("Cats FinalTagless with Twitter Future: ")

    import io.catbird.util.effect.futureToAsync

    val res1: F[String] =
      futureToAsync[F, String](Cases.twitter.doFuture[String]("twitter future"))
    val res11 = res1.attempt.map(_.left.map(_.getMessage.length))
    show(res11)

    val res2: F[Either[Int, String]] =
      futureToAsync[F, Either[Int, String]](
        Cases.twitter.doFutureEither[Int, String]("twitter future either")
      )
    show(res2)
  }

  def withScalaFuture[F[_]: Async](): Unit = {
    println("Cats Tagless Final with Scala Future: ")

    implicit val ex: ExecutionContext = ExecutionContext.global
    implicit val defaultCS: ContextShift[CatsIO] = CatsIO.contextShift(ex)

    // there is no mapper from Future to Async, or do it manually (see below)
    val res1: Future[String] = Cases.scala.doFuture[String]("scala future")
    val res11: F[String] = Async[F].async { cb =>
      res1.onComplete {
        case Failure(err)   => cb(Left(err))
        case Success(value) => cb(Right(value))
      }
    }

    val res111 = res11.attempt.map(_.left.map(_.getMessage.length))
    show(res111)

    // there is no mapper from Future to Async, uses cats IO as transit
    val res2: CatsIO[Either[Int, String]] =
      CatsIO.fromFuture(CatsIO(Cases.scala.doFutureEither[Int, String]("scala future either")))
    show(res2)
  }

  def withZio[F[_]: Async](): Unit = {
    println("Cats TaglessFinal with ZIO: ")

    import zio.interop.catz._
    implicit val runtime: BootstrapRuntime = new BootstrapRuntime {}

    val res1: Task[Either[RuntimeException, String]] =
      Cases.zio.doZio[RuntimeException, String]("zio").either
    val res11: F[Either[RuntimeException, String]] = res1.toIO.to[F]
    show(res11)
  }

  def show[F[_]: Async, T](f: F[T]): Unit =
    println(" - " + f.asInstanceOf[CatsIO[String]].unsafeRunSync())
}
