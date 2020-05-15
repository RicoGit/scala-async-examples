package errors

import cats.data.EitherT
import cats.{ApplicativeError, Functor, MonadError}
import cats.implicits._

import scala.language.higherKinds

object ErrorHelpers {

  object syntax {

    // implicit
    implicit class EitherInstance[E, T](value: Either[E, T]) {
      def errorAs[Err](implicit from: E => Err): Either[Err, T] = value.left.map(from)
    }

    implicit class FEitherInstance[F[_]: Functor, E, T](value: F[Either[E, T]]) {
      def errorAs[Err](implicit from: E => Err): F[Either[Err, T]] =
        Functor[F].map(value)(_.left.map(from.apply))
    }

    implicit class FInstance[F[_], T](value: F[T])(implicit ae: ApplicativeError[F, Throwable]) {
      def errorAs[Err](implicit from: Throwable => Err): F[Either[Err, T]] =
        Functor[F].map(value.attempt)(_.left.map(from.apply))
    }

    /**
      * Either ops
      * */
    implicit class EitherOps[Err, T](either: Either[Err, T]) {

      def rethrowAs[F[_]](leftMap: Err => Throwable)(implicit F: MonadError[F, Throwable]): F[T] =
        either match {
          case Left(err)    => leftMap(err).raiseError[F, T]
          case Right(value) => value.pure[F]
        }

      // todo consider rename
      def rethrow[F[_]](implicit F: MonadError[F, Throwable], ev: Err <:< Throwable): F[T] =
        either.rethrowAs[F](ev)

      // `toEitherT` method is already provided by [[cats.syntax.EitherOps]]. Use `cats.implicits._`

    }

    /**
      * F with Either ops
      * */
    implicit class MapErrorFEitherInstance[F[_], Err, T](eff: F[Either[Err, T]]) {

      def leftMap[NewErr](from: Err => NewErr)(implicit F: Functor[F]): F[Either[NewErr, T]] =
        Functor[F].map(eff)(_.left.map(from.apply))

      def toEitherT: EitherT[F, Err, T] = EitherT(eff)

      def toEitherT[NewErr](fn: Err => NewErr)(implicit F: Functor[F]): EitherT[F, NewErr, T] =
        EitherT(eff).leftMap(fn)

      /**
        * Add Error to `E` if `T` doesn't corresponded to predicate `f`.
        *
        * Example:
        * {{{
        * import cats.implicits._
        * import whisk.cats.implicits._
        *
        * val e1: List[Either[String, Int]] = List(Right(123), Left("abc"))
        * e1.ensure("error")(_ > 150)  // result: List[Either[String, Int]] = List(Left(error), Left(abc))
        *
        * val e2: IO[Either[String, Int]] = IO(Right(123))
        * e1.ensure("error")(_ > 150)  // result: IO[Either[String, Int]] = IO(Left(error))
        * }}}
        */
      def ensure[NewErr >: Err](
          onFailure: => NewErr
      )(f: T => Boolean)(implicit F: Functor[F]): F[Either[NewErr, T]] =
        eff.map(_.ensure(onFailure)(f))

      def errorAs[NewErr](implicit from: Err => NewErr, F: Functor[F]): F[Either[NewErr, T]] =
        Functor[F].map(eff)(_.left.map(from.apply))

      // for explicit mapping
      def rethrowAs(leftMap: Err => Throwable)(implicit F: MonadError[F, Throwable]): F[T] =
        eff.flatMap(_.rethrowAs[F](leftMap))

      // for implicit mapping error
      def rethrow_(implicit leftMap: Err => Throwable, F: MonadError[F, Throwable]): F[T] =
        eff.flatMap(_.rethrowAs[F](leftMap))

    }

    /** Helper methods to work with IO */
    implicit class RichFWithEither2[F[_], T](val eff: F[T]) extends AnyVal {
      def toEitherT[Err](implicit F: Functor[F]): EitherT[F, Err, T] = EitherT.liftF(eff)
    }

    implicit class EitherTOps[F[_], Err, T](value: EitherT[F, Err, T]) {

      def rethrowAs[NewErr <: Throwable](
          leftMap: Err => NewErr
      )(implicit F: MonadError[F, Throwable]): F[T] =
        value.leftMap(leftMap).rethrowT

//      def rethrowAs[NewErr <: Throwable](
//          implicit F: MonadError[F, Throwable],
//          mapErr: Err => NewErr
//      ): F[T] =
//        eff.flatMap(_.rethrow(mapErr))

      def rethrowOnly[E <: Err](
          from: E => Throwable
      )(implicit F: MonadError[F, Throwable]): EitherT[F, Err, T] =
        value.leftSemiflatMap {
          case err: E => from(err).raiseError[F, Err]
          case other  => other.pure[F]
        }
    }

    //    implicit class MapErrorFInstance[F[_], T](value: F[T])(
//        implicit ae: ApplicativeError[F, Throwable]
//    ) {
//      import cats.syntax.applicativeError._
//      def mapError[Err](from: Throwable => Err): F[Either[Err, T]] =
//        Functor[F].map(value.attempt)(_.left.map(from.apply))
//    }

  }

}
