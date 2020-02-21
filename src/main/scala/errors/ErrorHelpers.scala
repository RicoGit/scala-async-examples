package errors

import cats.{ApplicativeError, Functor}

object ErrorHelpers {

  object syntax {

    // implicit
    implicit class EitherInstance[E, T](value: Either[E, T]) {
      def errorAs[Err](implicit from: E => Err): Either[Err, T] = value.left.map(from.apply)
    }

    implicit class FEitherInstance[F[_]: Functor, E, T](value: F[Either[E, T]]) {
      def errorAs[Err](implicit from: E => Err): F[Either[Err, T]] =
        Functor[F].map(value)(_.left.map(from.apply))
    }

    implicit class FInstance[F[_], T](value: F[T])(implicit ae: ApplicativeError[F, Throwable]) {
      import cats.syntax.applicativeError._
      def errorAs[Err](implicit from: Throwable => Err): F[Either[Err, T]] =
        Functor[F].map(value.attempt)(_.left.map(from.apply))
    }

    // explicit

    implicit class MapErrorEitherInstance[E, T](value: Either[E, T]) {
      def mapError[Err](from: E => Err): Either[Err, T] = value.left.map(from.apply)
    }

    implicit class MapErrorFEitherInstance[F[_]: Functor, E, T](value: F[Either[E, T]]) {
      def mapError[Err](from: E => Err): F[Either[Err, T]] =
        Functor[F].map(value)(_.left.map(from.apply))
    }

    implicit class MapErrorFInstance[F[_], T](value: F[T])(implicit ae: ApplicativeError[F, Throwable]) {
      import cats.syntax.applicativeError._
      def mapError[Err](from: Throwable => Err): F[Either[Err, T]] =
        Functor[F].map(value.attempt)(_.left.map(from.apply))
    }

  }

}
