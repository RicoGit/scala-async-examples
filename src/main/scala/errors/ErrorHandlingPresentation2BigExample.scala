package errors

import java.util.UUID
import java.util.logging.Logger

import cats.data.ValidatedNec
import cats.effect.{IO, Timer}
import cats.implicits._
import errors.ErrorHandlingPresentation2BigExample.common.{Email, EmailLetter, EmailService}
import Email.InvalidEmail
import errors.ErrorHandlingPresentation2BigExample.dao.{
  Code,
  SecretsDao,
  Status,
  User,
  UserDao,
  UserId
}
import errors.ErrorHandlingPresentation2BigExample.service.UserService

import scala.concurrent.ExecutionContext
import scala.util.control.NoStackTrace
import ErrorHelpers.syntax._
import errors.ErrorHandlingPresentation2BigExample.api.{
  ApiError,
  BadRequest,
  Conflict,
  Forbidden,
  NotFound
}

object ErrorHandlingPresentation2BigExample {
  implicit val tm: Timer[IO] = IO.timer(ExecutionContext.global)

  object common {

    final class Email private (val address: String) extends AnyVal

    object Email {
      case class InvalidEmail(msg: String) // there is a special type for error

      def make(email: String): Either[InvalidEmail, Email] =
        Either.cond(
          email.length > 3 && email.contains("@"),
          right = new Email(email),
          left = InvalidEmail(s"invalid email=$email. Should contains @ and at least 3 symbols")
        )
    }

    case class EmailLetter(address: Email, subject: Option[String], text: String)

    trait EmailService {

      def sendEmail(letter: EmailLetter): IO[Unit]

    }

  }

  object dao {

    /**
      * User
      * */
    final class UserId private (val id: String) extends AnyVal
    object UserId {
      // there is no special type for error, just return String, it's ok here
      def make(id: String): Either[String, UserId] =
        Either.cond(id.length == 16, new UserId(id), "Id should contain exact 16 chars")
    }

    case class User(id: UserId, name: String, email: String, status: UserStatus = UserStatus.New)

    sealed trait UserStatus
    object UserStatus {
      case object New extends UserStatus
      case object Activation extends UserStatus
      case object Regular extends UserStatus
    }

    case class UserAlreadyExists(id: UserId)

    trait UserDao {

      def getUser(id: UserId): IO[Option[User]]

      def insertUser(name: String, email: Email): IO[Either[UserAlreadyExists, User]]

    }

    /**
      * Group
      * */
    case class GroupId(id: Long) extends AnyVal
    case class Group(id: GroupId, name: String, admin: User, members: List[User])

    sealed trait GroupDaoError
    case class GroupAlreadyExists(id: UserId)

    sealed trait AddToGroupError
    case object GroupNotFound // controversial, isn't it?
    case object VerificationIsNotComplete // group should be activated before
    case object MaxLimitExceeded // too many users are inside
    case object PrivateGroup // need an invite
    case object PermissionDenied

    trait GroupDao {

      def getGroup(id: GroupId): IO[Option[Group]]

      def insertGroup(user: Group): IO[Either[GroupAlreadyExists, Unit]]

      def allGroupsFor(user: UserId): IO[List[Group]]

      def addUserToGroup(user: UserId, groupId: GroupId): IO[Either[AddToGroupError, Group]]

    }

    /**
      * Secret
      * */
    case class Code(payload: Array[Byte]) extends AnyVal

    sealed trait Status
    // an alternative approach to encoding Errors (so simple cases)
    object Status {
      case object Ok
      case object Expired
      case object Invalid
    }

    trait SecretsDao {

      def generateAndSave(seed: String): IO[Code]

      def verifyCode(code: Code): IO[Status]

    }

  }

  object service {

    case class SendAskError(msg: String)

    trait EmailVerificationService {
      val emailSrv: EmailService
      val secretsDao: SecretsDao

      def sendAsk(email: Email): IO[Unit] = {
        def generateLink(code: Code): String = ???

        for {
          code <- secretsDao.generateAndSave(email.address)
          activationLetter = EmailLetter(email, Option("Activation Letter"), generateLink(code))
          _ <- emailSrv.sendEmail(activationLetter)
        } yield ()
      }

      def verifyCode(code: UUID): IO[Status]

    }

    case class UserAlreadyExists(email: Email)

    trait UserService {
      val userDao: UserDao
      val verificationSrv: EmailVerificationService
      val logger: Logger

      def getUser(id: UserId): IO[Option[User]] =
        userDao.getUser(id)

      def createNewUser(name: String, email: Email): IO[Either[UserAlreadyExists, User]] = {
        val result = for {
          // insert new User
          user <- userDao
            .insertUser(name, email)
            .toEitherT(_ => UserAlreadyExists(email))
          // send activation email
          _ <- verificationSrv
            .sendAsk(email)
            .handleError { _ =>
              // use will retry activation manually later
              logger.warning(s"Can't send activation email")
            }
            .toEitherT[UserAlreadyExists]

        } yield user

        result.value
      }

    }

  }

  object api {

    abstract class ApiError(val code: String, val message: Option[String])
        extends Exception(s"code: $code, $message")
        with NoStackTrace
    case class BadRequest(msg: String) extends ApiError("400", msg.some)
    case class Forbidden(msg: String) extends ApiError("403", msg.some)
    case class NotFound(msg: String) extends ApiError("404", msg.some)
    case class Conflict(msg: String) extends ApiError("409", msg.some)
    case class InternalError(msg: String) extends ApiError("500", msg.some)

    case class GetUserRequest(id: String)
    case class CreateUserRequest(email: String, name: String)

    trait UserApi {
      val userSrv: UserService

      def handle(request: GetUserRequest): IO[User] = {
        for {
          userId <- UserId.make(request.id).rethrowAs[IO](BadRequest)
          user <- userSrv.getUser(userId).map(_.toRight(NotFound("User not found"))).rethrow
        } yield user
      }

      def handle(request: CreateUserRequest): IO[User] = {
        def validateName(name: String): ValidatedNec[String, String] = ???

        for {
          email <- Email
            .make(request.email)
            .rethrowAs[IO] { case InvalidEmail(msg) => BadRequest(s"Bad email: $msg") }
          validName <- validateName(request.name).toEither
            .rethrowAs[IO](errors => BadRequest(errors.mkString_(",")))
          user <- userSrv
            .createNewUser(validName, email)
            .rethrowAs(_ => Conflict(s"User with the same email($email) already exists"))
        } yield user
      }

    }

  }

  object other {

    /**
      * Option vs UserNotFound
      * */
    // step 1

    def test(): Option[User] = ???

    // step 2

    case object ValidationErr

    def test2(): Either[ValidationErr.type, Option[User]] = ???

    // step 3

    sealed trait TestError
    case object ValidationError extends TestError
    case object UserNotFound extends TestError

    def test3(): Either[TestError, User] = ???

    // usages for step 2 Either[ValidationErr.type, Option[User]]

    // rethrow 1 error to error channel
    val res1: IO[Option[User]] = test2().rethrowAs[IO] {
      case ValidationErr => new Exception("Invalid User")
    }
    // rethrow all error to error channel
    val res2: IO[User] = test2()
      .rethrowAs[IO](ValidationErr => new Exception("Invalid User"))
      .flatMap(IO.fromOption(_)(new Exception("User not found")))
    // handle error
    val res3: IO[Option[User]] = test2().toOption.flatten.pure[IO]

    // usages for step 3 Either[TestError, User]

    // rethrow 1 error to error channel
    val res11: IO[Option[User]] = test3()
      .map(_.some)
      .leftFlatMap {
        case UserNotFound    => None.asRight
        case ValidationError => new Exception("Invalid User").asLeft
      }
      .rethrow[IO]
    // rethrow all errors to error channel
    val res12: IO[User] = test3()
      .rethrowAs[IO] {
        case UserNotFound    => new Exception("User not found")
        case ValidationError => new Exception("Invalid User")
      }
    // handle error
    val res13: IO[Option[User]] = test3().toOption.pure[IO]

    // When there are no others errors - Use Option
    // If you have Option value and atLeast 1 error - up to you )

    /**
      * Error super type
      * */
    case class ChatRoom(name: String)

    sealed trait RoomError

    sealed trait EnterRoomError extends RoomError

    sealed trait GetRoomError extends RoomError

    case class PermissionDenied(code: Long, msg: String) extends EnterRoomError with CreateRoomError
    case object RoomNotFound extends GetRoomError with EnterRoomError
    case object RoomBanned extends GetRoomError with EnterRoomError

    sealed trait CreateRoomError extends RoomError
    case object AlreadyExists extends CreateRoomError
    case object InvalidRoom extends CreateRoomError

    trait ChatRoomService {

      def getRoom(roomId: Long): IO[Either[GetRoomError, ChatRoom]]

      def enterRoom(roomId: Long, userId: String): IO[Either[EnterRoomError, ChatRoom]]

      def createRoom(room: ChatRoom): IO[Either[CreateRoomError, ChatRoom]]
    }

    def usage(): Unit = {
      val srv: ChatRoomService = ???

      srv.getRoom(1L).toEitherT {
        case RoomNotFound => ???
        case RoomBanned   => ???
      }

      srv.enterRoom(1L, "userId").toEitherT {
        case PermissionDenied(code, msg) => ???
        case RoomNotFound                => ???
        case RoomBanned                  => ???
      }

      srv.createRoom(ChatRoom("test")).toEitherT {
        case PermissionDenied(code, msg) => ???
        case AlreadyExists               => ???
        case InvalidRoom                 => ???
      }

      // if you don't care and want to throw
      val io: IO[ChatRoom] =
        srv
          .createRoom(ChatRoom("test"))
          .rethrowAs(err => new Exception(err.getClass.getSimpleName))
      // if you don't care and want to option
      val io2: IO[Option[ChatRoom]] =
        srv.createRoom(ChatRoom("test")).map(_.toOption)

    }

    trait ChatRoomServer {
      val roomSrv: ChatRoomService

      implicit val errorHandler: RoomError => ApiError = {
        case RoomNotFound | RoomBanned   => NotFound("Room is not exist or banned")
        case PermissionDenied(code, msg) => Forbidden(s"code: $code, msg")
        case AlreadyExists               => Conflict("bla  bla")
        case InvalidRoom                 => BadRequest("bla bla ")
      }

      def handleGet(req: String): IO[ChatRoom] =
        roomSrv.enterRoom(1L, "userId").rethrow_

      def handleCreate(req: String): IO[ChatRoom] =
        roomSrv.createRoom(ChatRoom("name")).rethrow_

      def handleEnter(req: String): IO[ChatRoom] =
        roomSrv.enterRoom(1L, "useId").rethrow_

      // there is no boilerplate

    }

  }

}
