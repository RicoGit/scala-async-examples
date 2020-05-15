package errors

import java.io.IOException
import java.sql.SQLException

import cats.data.{EitherT, Ior}
import cats.effect.{Async, IO}
import cats.implicits._

import scala.util.control.{NoStackTrace, NonFatal}
import ErrorHelpers.syntax._

import scala.util.{Random, Try}

/**
  * Code snippets for seconds presentation about error handling.
  * */
object ErrorHandlingPresentation2 {

  case class UserId(id: String) extends AnyVal
  case class User(id: UserId, name: String)

  case class GroupId(id: String) extends AnyVal
  case class Group(id: GroupId, name: String)

  sealed trait CreateUserError
  case class InvalidName(msg: String) extends CreateUserError
  case class UserAlreadyExists(msg: String) extends CreateUserError

  case class InvalidNameException(msg: String) extends Exception(msg) with NoStackTrace
  case class UserAlreadyExistsException(msg: String) extends Exception(msg) with NoStackTrace

  /** Function that validate User name */
  def isNameValid(name: String): Boolean = name != null && name.knownSize > 3 && name.knownSize < 32

  object Introduction {

    // simple control flow
    val x: Int = Random.nextInt()
    val y: Int = Random.nextInt()
    val res1: Int = x / y
    println(res1.toString)

    val x2: Int = Random.nextInt()
    val y2: Int = Random.nextInt()
    val res2: String =
      if (y2 == 0) "Divide by zero!" else (x2 / y2).toString

    // simple flow with monad
    val app: IO[Int] = for {
      x <- IO(Random.nextInt())
      y <- IO(Random.nextInt())
      _ <- if (y == 0) IO.raiseError(new Exception("Divide by zero!")) else IO.unit
      res <- IO(x / y)
    } yield res

    app.recover { case err: Exception => -1 }

    // other useless code needed for slides

    def createUserDao(name: String): User = {
      if (name == "test") throw UserAlreadyExistsException("This name is occupied")
      User(UserId("id"), name)
    }

    def createUser(name: String): User = {
      if (isNameValid(name)) throw new IllegalArgumentException("Invalid name")
      createUserDao(name)
    }
    case class BadRequestError(msg: String) extends Exception("400")

    val user1 = createUser("test")

    val user: User = Try(createUser("test")).adaptError {
      case invalidName: IllegalArgumentException =>
        BadRequestError(invalidName.getMessage) // http code: 400
    }.get

  }

  // a try to handle all exception explicitly (not very successful)
  object OneExplicitErrorChannel {

    def getUser(id: UserId): IO[User] = ??? // errors hided
    def addUser(id: GroupId, user: User): IO[Unit] = ??? // errors hided

    def liftErrors(id: UserId): Unit = {

      val res: IO[Either[String, Int]] = for {
        right1 <- IO(Right(1))
        left <- IO(Left("error"))
        right2 <- IO(Right(2))
      } yield right2 // returns IO(Right(2))

      val res1 = for {
        right1 <- IO(Either.right[String, Int](1))
        left <- IO(right1 match {
          case Right(_) => Left("error")
          case left     => left
        })
        right2 <- IO(left match {
          case Right(_) => Either.right[String, Int](2)
          case left     => left
        })
      } yield right2 // returns IO(Left("error")) as expected

      // eitherT give as short circuit
      val io: IO[User] = ???
      // lift errors to explicit errors channel
      io.attempt.toEitherT
      // or shorter just
      io.attemptT
    }

    sealed trait Errors
    case object UserNotFound extends Errors
    case class SqlError(cause: SQLException) extends Errors
    case class IOError(cause: IOException) extends Errors
    case class OtherError(cause: Throwable) extends Errors

    // really terrible decision to handle all possible error explicitly
    def addUserToGroup(groupId: GroupId, userId: UserId): IO[Either[Errors, Unit]] = {
      val result = for {
        user <- getUser(userId)
        _ <- addUser(groupId, user)
      } yield ()

      result.attemptT.leftMap {
        case dbError: SQLException if dbError.getErrorCode == 0 => UserNotFound
        case dbError: SQLException                              => SqlError(dbError)
        case ioError: IOException                               => IOError(ioError)
        case NonFatal(other)                                    => OtherError(other)
      }.value
    }

    sealed trait ApiError
    // oh common it looks weird and verbose
    def run(): IO[Either[ApiError, Unit]] = {
      addUserToGroup(GroupId(""), UserId("")).leftMap {
        case UserNotFound      => ???
        case SqlError(cause)   => ???
        case IOError(cause)    => ???
        case OtherError(cause) => ???
      }
    }

  }

  /**
    * Creates User by name, may return Error explicitly as well as failed IO with
    * different exceptions like IOException
    * */
  def createUserIOEither(name: String): IO[Either[InvalidName, User]] = ???

  /**
    * Exception-based approach (throws Exception, java-style)
    * When we're witting simple procedure code we have one error channel (throw new Exception)
    *  */
  object OneChannelsWithException {

    /** Creates User by name, may throw many different exceptions like IOException. */
    def createUser(name: String): User = ???

    /** Validates input name and creates and returns new User */
    def createUserByName1(name: String): User = {
      require(isNameValid(name), "Wrong user name") // throws IllegalArgumentException
      createUser(name) // throws domain errors, unexpected errors, or nothing - we don't know
    }

    /** Validates input name and creates and returns new User, additionally throws domain exception */
    def createUserByName2(name: String): User = {
      if (isNameValid(name)) {
        throw InvalidNameException("Wrong user name") // throws specific domain error, looks better
      }
      createUser(name) // throws domain errors, unexpected errors, or nothing - we don't know
    }
  }

  /**
    * MonadFails-based approach (raise Exception in IO, scala-style)
    * When we're witting simple async code we have one error channel as well (Future.failed, IO.raise)
    *  */
  object OneChannelsWithIO {

    /** Creates User by name, may return failed IO with different exceptions like IOException. */
    def createUser(name: String): IO[User] = ???

    /** Validates input name and creates and returns new User */
    def createUserByName1(name: String): IO[User] = {
      IO.pure(name)
        .ensure(new IllegalArgumentException("Wrong user name"))(isNameValid) // raises IllegalArgumentException
        .flatMap(createUser) // raises domain errors, unexpected errors, or nothing - we don't know
    }

    /** Validates input name and creates and returns new User, additionally throws domain exception */
    def createUserByName2(name: String): IO[User] = {
      IO.pure(name)
        .ensure(InvalidNameException("Wrong user name"))(isNameValid) // raises IllegalArgumentException
        .flatMap(createUser) // raises domain errors, unexpected errors, or nothing - we don't know
    }

  }

  /**
    * Explicit errors handling with `IO[Either[E, T]]`
    * When we're witting async code and we have 2 error channels:
    *  - the first one into Effect (IO.raise)
    *  - the second one into Either (Left value)
    * */
  object TwoChannelsWithIO {

    /** Creates User by name, may return domain error explicitly or failed IO with Exception */
    def createUser(name: String): IO[Either[CreateUserError, User]] = ???

    // bad code example
    def validateName1(name: String): IO[Either[CreateUserError, String]] =
      EitherT
        .cond[IO](isNameValid(name), right = name, left = InvalidName("Wrong user name"))
        .leftWiden[CreateUserError]
        .value

    // good code example
    def validateName(name: String): Either[InvalidName, String] =
      Either.cond(isNameValid(name), right = name, left = InvalidName("Wrong user name"))

    /** Validates input name and creates and returns new User */
    def createUserByName1(name: String): IO[Either[CreateUserError, User]] = {
      EitherT
        .cond[IO](isNameValid(name), right = name, left = InvalidName("Wrong user name")) // returns domain error explicitly
        .flatMap(name => createUser(name).toEitherT.leftWiden[CreateUserError]) // returns domain errors or success value,   or unexpected errors, we know enough
        .value
    }

    /** Validates input name and creates and returns new User */
    def createUserByName2(name: String): IO[Either[CreateUserError, User]] = {
      val result = for {
        _ <- EitherT
          .cond[IO](isNameValid(name), right = name, left = InvalidName("Wrong user name"))
        user <- createUser(name).toEitherT.leftWiden[CreateUserError]
      } yield user
      result.value
    }

    // downgrade explicit errors to implicit

    // all

    val io: EitherT[IO, CreateUserError, User] = ???

    val io10: EitherT[IO, CreateUserError, User] = ???

    // I don't want to handle any error, lets throw them
    val io11: IO[User] = io.leftMap(err => new Exception(err.toString)).rethrowT

    // or shorter (this method will comes from our error handling framework)
    val io12: IO[User] = io.rethrowAs(err => new Exception(err.toString))

    // specific

    // i don't want to handle UserAlreadyExists error (looks verbose)
    val io1: EitherT[IO, CreateUserError, User] = io.leftFlatMap {
      case err: InvalidName => EitherT.leftT[IO, User](err)
      case err: UserAlreadyExists =>
        IO.raiseError[Either[CreateUserError, User]](new Exception(err.toString)).toEitherT
    }

    // or shorter (this method will comes from our error handling framework)
    val io2: EitherT[IO, CreateUserError, User] =
      io.rethrowOnly[UserAlreadyExists](err => new Exception(err.toString))

  }

}
