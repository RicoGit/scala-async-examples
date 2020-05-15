package errors

import cats.data.{EitherT, Ior}
import cats.effect.{Async, IO}
import zio.{Task, ZIO}

import scala.concurrent.Future
import scala.util.control.Exception._
import scala.util.control.{NoStackTrace, NonFatal}
import scala.util.{Failure, Try}

// Error handling for PURE function.
object ErrorHandlingPresentation1 {

  // Bad! Non-total definition. The method signature is lying. Error isn't reflected in the signature of the method.
  def parseString(str: String): Int = str.toInt

  // Ok is some situation! Total definition. Acceptable when no need for explanation.
  def parseStringOpt(str: String): Option[Int] = Try(str.toInt).toOption

  // Not so good! Total definition. But errors `type` isn't reflected in the method's signature. And error have to be to be Throwable.
  def parseStringTry(str: String): Try[Int] = Try(str.toInt)

  // Fullest definition! Success and failure path are reflected in the method's signature.
  def parseStringEither(str: String): Either[ParseErr, Int] =
    Try(str.toInt).toEither.left.map(ex => ParseErr(ex.getMessage))

  def parseStringEither2(str: String): Either[ParseErr, Int] =
    nonFatalCatch[Int].either(str.toInt).left.map(ex => ParseErr(ex.getMessage))

  // Conclusion for PURE function:
  // Use return type T - only when the is no way to fail
  // Use return type Option[T] - only when no need for explanation of fail.
  // Use return type Either[E, T] - in all cases except the first two.

  case class ParseErr(msg: String)

  /** Don't catch Throwable */
  def catchExceptions() {

    def fn(): String = throw new Exception()

    // BAD! don't catch Throwable
    try {
      fn()
    } catch { case err: Throwable => println(err) }

    // use catch NonFatal
    try {
      fn()
    } catch { case NonFatal(err) => println(err) }

    // or better scala.util.Try
    Try(fn()).failed.map(println)

    // or scala.util.control.Exception
    nonFatalCatch[String].either(fn()).left.map(println)
  }

  /** Avoid creating an Exception if you return errors without throwing. */
  def createExceptions() {

    // BAD. When error created it'll constructs stack trace. In many cases you don't need it.
    case class MyError1(msg: String) extends Throwable(msg)

    // Better, but err.getMessage() doesn't return your msg.
    case class MyError2(msg: String) extends NoStackTrace

    // Better, but verbose, do it if the inheritance from the Throwable is unavoidable
    case class MyError3(msg: String) extends Throwable(msg) {
      override def fillInStackTrace(): Throwable = this
    }

    // Good (baseline)
    case class MyError4(msg: String) extends Exception(msg) with NoStackTrace

    // Perfect (without Throwable)
    case class MyError(msg: String)
    case object MyErrorObject

  }

  /** Prefer to raise errors in Effect instead of throwing exceptions */
  def effect[F[_]: Async](): Unit = {

    val _try: Try[Int] = Failure(new RuntimeException("boom!"))
    // or
    val future: Future[Int] = Future.failed(new RuntimeException("boom!"))
    // or
    val async: F[Int] = Async[F].raiseError[Int](new RuntimeException("boom!"))
    // or
    val io: IO[Int] = IO.raiseError[Int](new RuntimeException("boom!"))
    // or
    val task: Task[Int] = ZIO.fail(new RuntimeException("boom!"))
  }

  /** Use 'new types' and 'smart constructors' for getting better type-safety */
  def smartConstructors(): Unit = {

    case class Email private (value: String)

    object Email {

      case class InvalidEmail(email: String) {
        def msg: String = s"Email=$email is not valid "
      }

      def apply(email: String): Either[InvalidEmail, Email] =
        Either.cond(isValid(email), new Email(email), InvalidEmail(email))

      def isValid(email: String): Boolean = ???
    }

  }

  /** Return errors explicitly. Let a method signature reflects errors */
  def explicitErrors(): Unit = {
    case class Json()

    // BAD! How should we handle error cases?
    def parse1(json: String): Json = ???

    // Good total function
    def parse2(json: String): Either[ParseErr, Json] = ???

    // Good total function
    def parse3(json: String): zio.IO[ParseErr, Json] = ???

    // Ok, but verbose
    def parse4(json: String): IO[Either[ParseErr, Json]] = ???

    // Ok, but verbose, the same as parse4, but with monad transformer
    def parse5(json: String): EitherT[IO, ParseErr, Json] = ???

  }

  /** Use user-defined error*/
  def userDefinedErrors(): Unit = {
    import cats.implicits._

    val logger = new { def error(msg: String, err: Option[Throwable] = None) = ??? }

    case class User(id: Long, name: String)
    val user = User(1, "Name")

    sealed trait ApiError
    case class BadRequest(msg: String) extends ApiError
    case class InternalServerError(msg: String) extends ApiError
    case class NotFound(msg: String) extends ApiError

    sealed trait EmailError

    sealed trait UserServiceError
    case class InvalidUser(msg: String) extends UserServiceError
    case class UserAlreadyExists() extends UserServiceError
    case class UserNotFound() extends UserServiceError
    case class InternalError(cause: Throwable) extends UserServiceError

    def getUser(id: Long): zio.IO[UserServiceError, User] = ???
    def createUser(user: User): zio.IO[UserServiceError, User] = ???
    def sendEmail(user: User): zio.IO[EmailError, Unit] = ???

    val result: zio.IO[ApiError, Unit] = for {
      user <- createUser(user)
        .catchSome {
          case UserAlreadyExists() => getUser(user.id)
        }
        .mapError {
          case InvalidUser(msg)    => BadRequest(msg)
          case UserAlreadyExists() => BadRequest("already exists")
          case InternalError(err) =>
            logger.error(s"error while creating user=$user", err.some)
            InternalServerError("ups something goes wrong")
          case other =>
            InternalServerError("ups something goes wrong")
        }
      _ <- sendEmail(user).mapError { err =>
        logger.error(s"error while creating user=$user")
        InternalServerError("ups something goes wrong")
      }

    } yield ()

  }

  def monadError(): Unit = {
    import cats.implicits._

    // error type vanished
    val io: IO[Int] = IO.raiseError(new IllegalArgumentException())
    // we can get back Throwable only
    val attempt: IO[Either[Throwable, Int]] = io.attempt
    // and hide error back
    val end: IO[Int] = attempt.rethrow

  }

  def iorType(): Unit = {
    import cats.implicits._

    val error = Ior.leftNec[ParseErr, String](ParseErr("error"))
    val value = "value1".rightIor
    val both = Ior.bothNec[ParseErr, String](ParseErr("warning"), "value2")

    val successIor = (value, both).mapN { case (v, b) => s"$v $b" }
    println(successIor) // Both(Chain(ParseErr(warning)),value1 value2)
    val errorIor = (error, value, both).mapN { case (e, v, b) => s"$e $v $b" }
    println(error) // Left(Chain(ParseErr(error)))
  }

  def zioError(): Unit = {
    import cats.implicits._

    val errorIo: zio.IO[ParseErr, String] = zio.IO.fail(ParseErr("error"))
    val valueIo: zio.IO[Throwable, String] = zio.IO.succeed("value")

    val result: zio.IO[ParseErr, String] = for {
      value <- valueIo.mapError(err => ParseErr(err.getMessage))
      _ <- errorIo
    } yield value

    // the same with IO[Either[ParseError, String]]

    val errorIo2: IO[Either[ParseErr, String]] = IO(ParseErr("error").asLeft)
    val valueIo2: IO[Either[Throwable, String]] = IO("value".asRight)

    val result2: IO[Either[ParseErr, String]] = for {
      value2 <- valueIo2.map(_.left.map(err => ParseErr(err.getMessage)))
      _ <- errorIo2
    } yield value2

    // the same with EitherT[IO, ParseError, String]

    val errorIo3: EitherT[IO, ParseErr, String] = EitherT.fromEither(ParseErr("error").asLeft)
    val valueIo3: EitherT[IO, Throwable, String] = EitherT(IO("value".asRight))

    val result3: EitherT[IO, ParseErr, String] = for {
      value3 <- valueIo3.leftMap(err => ParseErr(err.getMessage))
      _ <- errorIo3
    } yield value3

  }

  def errorHelper(): Unit = {
    import cats.implicits._
    import errors.ErrorHelpers.syntax._

    implicit val from: ParseErr => Throwable = err => new Exception(err.msg)

    val e1: Either[ParseErr, String] = ParseErr("error").asLeft
    val e2: Either[Exception, String] = new Exception("error").asLeft

    val result = for {
      v1 <- e1.errorAs[Throwable]
      v2 <- e1.leftMap(err => new Exception(err.msg))
      v3 <- e2
    } yield ()

  }

  /** F[ Either[E, T] ] is bad composable */
  def FWithEither(): Unit = {
    import cats.implicits._

    val action1: IO[Either[ParseErr, String]] = IO("value".asRight)
    val action2: IO[Either[ParseErr, String]] = IO(ParseErr("parse err").asLeft)
    val action3: IO[Either[ParseErr, String]] = IO.raiseError(new Exception("exception"))

    val res = for {
      value <- action1
      value2 <- value.map(_ => action2).getOrElse(IO(value))
      value3 <- value2 match {
        case err @ Left(_) => IO(err)
        case Right(value)  => action3
      }
    } yield value3

    println(res.unsafeRunSync()) // return Left(ParseErr(parse err))

  }

  def eitherT(): Unit = {
    import cats.implicits._

    val right: EitherT[IO, ParseErr, String] = EitherT.right(IO("value"))
    val left: EitherT[IO, ParseErr, String] = EitherT.leftT(ParseErr("parse err"))
    val error: EitherT[IO, ParseErr, String] = EitherT(
      IO.raiseError[Either[ParseErr, String]](new Exception("exception"))
    )

    val res1 = for {
      value <- right
      leftErr <- left
    } yield println("never prints")

    val res2 = for {
      value <- right
      err <- error
    } yield println("never prints")

    println(res1.value.unsafeRunSync()) // return Left(ParseErr(parse err))
    println(res2.value.unsafeRunSync()) // throws exception

  }

}
