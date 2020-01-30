package interoperabillity

import cats.effect.{ContextShift, IO => CatsIO}
import zio.{DefaultRuntime, Task}

import scala.concurrent.ExecutionContext

/** Interop between Cats IO and other stuff. **/
object CatsIOInterop {

  def withFinalTagless(): Unit = {
    println("Cats IO with cats tagless final: ")

    val res1: CatsIO[String] = Cases.taglessFinal.doAsync[CatsIO, String]("async")
    val res11: CatsIO[Either[Int, String]] = res1.attempt.map(_.left.map(_.getMessage.length))
    show(res11)

    val res2: CatsIO[Either[Int, String]] =
      Cases.taglessFinal.doAsyncEither[CatsIO, Int, String]("async either")
    show(res2)

    {
      implicit val defaultCS: ContextShift[CatsIO] = CatsIO.contextShift(ExecutionContext.global)
      val res3: CatsIO[Either[Int, String]] =
        Cases.taglessFinal.doEffect[CatsIO, Int, String]("effect")
      show(res3)
    }
  }

  def withMonix(): Unit = {
    println("Cats IO with Monix: ")

    import monix.execution.Scheduler.Implicits.global

    val res1: CatsIO[String] = Cases.monix.doMonix[String]("monix task").to[CatsIO]
    val res11 = res1.attempt.map(_.left.map(_.getMessage.length))
    show(res11)

    val res2: CatsIO[Either[Int, String]] =
      Cases.monix.doMonixEither[Int, String]("monix task either").to[CatsIO]
    show(res2)

  }

  def withTwitterFuture(): Unit = {
    println("Cats IO with Twitter Future: ")

    import io.catbird.util.effect.futureToAsync

    val res1: CatsIO[String] =
      futureToAsync[CatsIO, String](Cases.twitter.doFuture[String]("twitter future"))
    val res11 = res1.attempt.map(_.left.map(_.getMessage.length))
    show(res11)

    val res2: CatsIO[Either[Int, String]] =
      futureToAsync[CatsIO, Either[Int, String]](Cases.twitter.doFutureEither[Int, String]("twitter future either"))
    show(res2)
  }

  def withScalaFuture(): Unit = {
    println("Cats IO with Scala Future: ")

    implicit val defaultCS: ContextShift[CatsIO] = CatsIO.contextShift(ExecutionContext.global)

    val res1: CatsIO[String] =
      CatsIO.fromFuture(CatsIO(Cases.scala.doFuture[String]("scala future")))
    val res11 = res1.attempt.map(_.left.map(_.getMessage.length))
    show(res11)

    val res2: CatsIO[Either[Int, String]] =
      CatsIO.fromFuture(CatsIO(Cases.scala.doFutureEither[Int, String]("scala future either")))
    show(res2)
  }

  def withZio(): Unit = {
    println("Cats IO with ZIO: ")

    import cats.effect.implicits._
    import zio.interop.catz._
    implicit val runtime: DefaultRuntime = new DefaultRuntime {}

    val res1: Task[Either[RuntimeException, String]] =
      Cases.zio.doZio[RuntimeException, String]("zio").either
    val res11: CatsIO[Either[RuntimeException, String]] = res1.toIO
    show(res11)
  }

  def show[T](io: CatsIO[T]): Unit =
    println(" - " + io.unsafeRunSync())
}
