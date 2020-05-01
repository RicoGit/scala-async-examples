package interoperabillity

import cats.effect.{Async, ConcurrentEffect, IO => CatsIO}
import com.twitter.util.{Future => TFuture}
import monix.eval.{Task => MonixTask}
import zio.IO

import scala.concurrent.Future

object Cases {

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
  object monix {

    def doMonix[T](t: T): MonixTask[T] = MonixTask(t)

    def doMonixEither[E, T](t: T): MonixTask[Either[E, T]] = MonixTask(Right(t))

  }

  //  twitter future
  object twitter {

    def doFuture[T](t: T): TFuture[T] = TFuture.value(t)

    def doFutureEither[E, T](t: T): TFuture[Either[E, T]] = TFuture(Right(t))

  }
  // scala future

  object scala {

    def doFuture[T](t: T): Future[T] = Future.successful(t)

    def doFutureEither[E, T](t: T): Future[Either[E, T]] = Future.successful(Right(t))

  }

  // ZIO

  object zio {

    def doZio[E, T](t: T): IO[E, T] = IO.fromEither(Right(t))

    // ZIO doesn't need case with Either. Either already inside ZIO

  }

}
