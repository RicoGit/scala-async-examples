package interoperabillity

import cats.effect.{ContextShift, IO                            => CatsIO}
import com.twitter.util.{Await, Duration, Return, Throw, Future => TFuture, Promise => TPromise}
import monix.eval.Task
import monix.execution.Scheduler
import zio.DefaultRuntime

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success}

/** Interop between Twitter Future and other stuff. **/
object TwitterFuturesInterop {

  /**
    * Converter from a Twitter Future to a Scala Future
    * Official way: https://twitter.github.io/util/guide/util-cookbook/futures.html#conversions-between-twitter-s-future-and-scala-s-future
    * */
  implicit class RichTwitterFuture[A](val tf: TFuture[A]) extends AnyVal {
    def asScalaFuture(implicit e: ExecutionContext): Future[A] = {
      val promise: Promise[A] = Promise()
      tf.respond {
        case Return(value)    => promise.success(value)
        case Throw(exception) => promise.failure(exception)
      }
      promise.future
    }
  }

  /**
    * Converter from a Scala Future to a Twitter Future
    * Official way: https://twitter.github.io/util/guide/util-cookbook/futures.html#conversions-between-twitter-s-future-and-scala-s-future
    */
  implicit class RichScalaFuture[A](val sf: Future[A]) extends AnyVal {
    def asTFuture(implicit e: ExecutionContext): TFuture[A] = {
      val promise: TPromise[A] = new TPromise[A]()
      sf.onComplete {
        case Success(value)     => promise.setValue(value)
        case Failure(exception) => promise.setException(exception)
      }
      promise
    }
  }

  /** Converter from IO to Twitter Future */
  implicit class RichIO[A](val io: CatsIO[A]) extends AnyVal {
    def asTFuture(): TFuture[A] = {
      val promise: TPromise[A] = new TPromise[A]()
      io.unsafeRunAsync {
        case Right(value)    => promise.setValue(value)
        case Left(exception) => promise.setException(exception)
      }
      promise
    }
  }

  /** Converter from monix Task to Twitter Future */
  implicit class RichMonixTask[A](val task: Task[A]) extends AnyVal {
    def asTFuture(implicit scheduler: Scheduler): TFuture[A] = {
      val promise: TPromise[A] = new TPromise[A]()
      task.attempt.map {
        case Right(value)    => promise.setValue(value)
        case Left(exception) => promise.setException(exception)
      }.runAsyncAndForget
      promise
    }
  }

  def withFinalTagless(): Unit = {
    println("Twitter Future with cats tagless final: ")

    // there is no implementation Async[TFuture], uses cats IO as transit

    val res1: TFuture[String] = Cases.taglessFinal.doAsync[CatsIO, String]("async").asTFuture()
    val res11: TFuture[Either[Int, String]] =
      res1.liftToTry
        .map(_.asScala.toEither.left.map(_.getMessage.length))
    show(res11)

    val res2: TFuture[Either[Int, String]] =
      Cases.taglessFinal.doAsyncEither[CatsIO, Int, String]("async either").asTFuture()
    show(res2)

    {
      implicit val defaultCS: ContextShift[CatsIO] = CatsIO.contextShift(ExecutionContext.global)
      val res3: TFuture[Either[Int, String]] =
        Cases.taglessFinal.doEffect[CatsIO, Int, String]("effect").asTFuture()
      show(res3)
    }
  }

  def withCatsIO(): Unit = {
    println("Twitter Future with cats effect IO: ")

    implicit val ex: ExecutionContext = ExecutionContext.global

    val res1: TFuture[String] = Cases.catsIO.doIo[String]("cats io").asTFuture()
    val res11: TFuture[Either[Int, String]] =
      res1.liftToTry
        .map(_.asScala.toEither.left.map(_.getMessage.length))
    show(res11)

    val res2: TFuture[Either[Int, String]] =
      Cases.catsIO.doIoEither[Int, String]("cats io either").asTFuture()
    show(res2)
  }

  def withMonix(): Unit = {
    println("Twitter Future with Monix: ")

    import monix.execution.Scheduler.Implicits.global

    val res1: TFuture[String] = Cases.monix.doMonix[String]("monix task").asTFuture
    val res11: TFuture[Either[Int, String]] =
      res1.liftToTry
        .map(_.asScala.toEither.left.map(_.getMessage.length))
    show(res11)

    val res2: TFuture[Either[Int, String]] =
      Cases.monix.doMonixEither[Int, String]("monix task either").asTFuture
    show(res2)

  }

  def withScalaFuture(): Unit = {
    println("Twitter Future with Scala Future: ")

    implicit val ex: ExecutionContext = ExecutionContext.global

    val res1: TFuture[String] = Cases.scala.doFuture[String]("twitter future").asTFuture
    val res11: TFuture[Either[Int, String]] =
      res1.liftToTry
        .map(_.asScala.toEither.left.map(_.getMessage.length))
    show(res11)

    val res2: TFuture[Either[Int, String]] =
      Cases.scala.doFutureEither[Int, String]("twitter future either").asTFuture
    show(res2)
  }

  def withZio(): Unit = {
    println("Twitter Future with ZIO: ")

    val runtime: DefaultRuntime = new DefaultRuntime {}
    import zio.interop.twitter._

    val res1: TFuture[String] =
      runtime.unsafeRunToTwitterFuture(Cases.zio.doZio[RuntimeException, String]("zio"))
    val res11: TFuture[Either[Int, String]] =
      res1.liftToTry.map(_.asScala.toEither.left.map(_.getMessage.length))
    show(res11)

  }

  def show[T](future: TFuture[T]): Unit =
    println(" - " + Await.result(future, Duration.fromSeconds(5)))
}
