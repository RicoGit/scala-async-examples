package interoperabillity

import cats.effect.{ContextShift, IO => CatsIO}
import com.twitter.util.{Return, Throw, Future => TFuture}
import zio.{DefaultRuntime, IO, Task, UIO}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future, Promise}

/** Interop between Scala Future and other stuff. **/
object ScalaFutureInterop {

  def withFinalTagless(): Unit = {
    println("Scala Future with cats tagless final: ")

    implicit val ex: ExecutionContext = ExecutionContext.global

    // there is no implementation Async[Future], uses cats IO as transit

    val res1: Future[String] = Cases.taglessFinal.doAsync[CatsIO, String]("async").unsafeToFuture()
    // there is no function to lift error to value like attempt or either
    val res11: Future[Either[Int, String]] =
      res1
        .map(Right.apply)
        .recoverWith(err => Future.successful(Left(err.getMessage.length)))
    show(res11)

    val res2: Future[Either[Int, String]] =
      Cases.taglessFinal.doAsyncEither[CatsIO, Int, String]("async either").unsafeToFuture()
    show(res2)

    {
      implicit val defaultCS: ContextShift[CatsIO] = CatsIO.contextShift(ex)
      val res3: Future[Either[Int, String]] =
        Cases.taglessFinal.doEffect[CatsIO, Int, String]("effect").unsafeToFuture()
      show(res3)
    }
  }

  def withCatsIO(): Unit = {
    println("Scala Future with cats effect IO: ")

    implicit val ex: ExecutionContext = ExecutionContext.global

    val res1: Future[String] = Cases.catsIO.doIo[String]("cats io").unsafeToFuture()
    // there is no function to lift error to value like attempt or either
    val res11: Future[Either[Int, String]] = res1
      .map(Right.apply)
      .recoverWith(err => Future.successful(Left(err.getMessage.length)))
    show(res11)

    val res2: Future[Either[Int, String]] =
      Cases.catsIO.doIoEither[Int, String]("cats io either").unsafeToFuture()
    show(res2)
  }

  def withMonix(): Unit = {
    println("Scala Future with Monix: ")

    import monix.execution.Scheduler.Implicits.global

    val res1: Future[String] = Cases.monix.doMonix[String]("monix task").runToFuture
    // there is no function to lift error to value like attempt or either
    val res11: Future[Either[Int, String]] = res1
      .map(Right.apply)
      .recoverWith(err => Future.successful(Left(err.getMessage.length)))
    show(res11)

    val res2: Future[Either[Int, String]] =
      Cases.monix.doMonixEither[Int, String]("monix task either").runToFuture
    val res22: Future[Either[Int, String]] = res2
    show(res22)

  }

  /**
    * Convert from a Twitter Future to a Scala Future
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

  def withTwitterFuture(): Unit = {
    println("Scala Future with Twitter Future: ")

    implicit val ex: ExecutionContext = ExecutionContext.global

    val res1: Future[String] = Cases.twitter.doFuture[String]("twitter future").asScalaFuture
    val res11: Future[Either[Int, String]] = res1
      .map(Right.apply)
      .recoverWith(err => Future.successful(Left(err.getMessage.length)))
    show(res11)

    val res2: Future[Either[Int, String]] =
      Cases.twitter.doFutureEither[Int, String]("twitter future either").asScalaFuture
    show(res2)
  }

  def withZio(): Unit = {
    println("Scala Future with ZIO: ")

    val runtime: DefaultRuntime = new DefaultRuntime {}
    implicit val ex: ExecutionContext = ExecutionContext.global

    val res1: Future[String] =
      runtime.unsafeRun(Cases.zio.doZio[RuntimeException, String]("zio").toFuture)
    val res11: Future[Either[Int, String]] = res1
      .map(Right.apply)
      .recoverWith(err => Future.successful(Left(err.getMessage.length)))
    show(res11)

  }

  def show[T](future: Future[T]): Unit =
    println(" - " + Await.result(future, Duration.Inf))
}
