package interoperabillity

import cats.effect.{ContextShift, IO => CatsIO}
import zio.DefaultRuntime

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

/** Interop between Scala Future and other stuff. **/
object ScalaFuturesInterop {

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
    show(res2)

  }

  def withTwitterFuture(): Unit = {
    println("Scala Future with Twitter Future: ")

    import interoperabillity.TwitterFuturesInterop.RichTwitterFuture
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
