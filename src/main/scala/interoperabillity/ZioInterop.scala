package interoperabillity

import zio.internal.Platform
import zio.{IO, Runtime, Task}

/** Interop between ZIO and other stuff each call should be mapped to IO[E, T] **/
object ZioInterop {

  val runtime: Runtime[Unit] = new Runtime[Unit]() {
    override val environment: Unit = ()
    override val platform: Platform = Platform.default
  }

  def withFinalTagless(): Unit = {
    println("ZIO with cats tagless final: ")

    import zio.interop.catz._

    val res1: Task[String] = Cases.taglessFinal.doAsync[Task, String]("async")
    val res11: IO[Int, String] = res1.mapError(_.getMessage.length)
    show(res11)(runtime)

    val res2: Task[Either[Int, String]] =
      Cases.taglessFinal.doAsyncEither[Task, Int, String]("async either")
    val res22: IO[Int, String] = res2.mapError(_.getMessage.length).absolve
    show(res22)(runtime)

    {
      implicit val r = runtime
      val res3: Task[Either[Int, String]] =
        Cases.taglessFinal.doEffect[Task, Int, String]("effect")
      val res33: IO[Int, String] = res3.mapError(_.getMessage.length).absolve
      show(res33)(runtime)
    }
  }

  def withCatsIO(): Unit = {
    println("ZIO with cats effect IO: ")

    import zio.interop.catz._

    val res1: Task[String] = Cases.catsIO.doIo[String]("cats io").to[Task]
    val res11: IO[Int, String] = res1.mapError(_.getMessage.length)
    show(res11)(runtime)

    val res2: Task[Either[Int, String]] =
      Cases.catsIO.doIoEither[Int, String]("cats io either").to[Task]
    val res22: IO[Int, String] = res2.mapError(_.getMessage.length).absolve
    show(res22)(runtime)
  }

  def withMonix(): Unit = {
    println("ZIO with Monix: ")

    import monix.execution.Scheduler.Implicits.global
    import zio.interop.monix._

    val res1: Task[String] = IO.fromTask(Cases.monix.doMonix[String]("monix task"))
    val res11: IO[Int, String] = res1.mapError(_.getMessage.length)
    show(res11)(runtime)

    val res2: Task[Either[Int, String]] =
      IO.fromTask(Cases.monix.doMonixEither[Int, String]("monix task either"))
    val res22: IO[Int, String] = res2.mapError(_.getMessage.length).absolve
    show(res22)(runtime)

  }

  def withTwitterFuture(): Unit = {
    println("ZIO with Twitter Future: ")

    import zio.interop.twitter._

    val res1: Task[String] =
      Task.fromTwitterFuture(Task(Cases.twitter.doFuture[String]("twitter future")))
    val res11: IO[Int, String] = res1.mapError(_.getMessage.length)
    show(res11)(runtime)

    val res2: Task[Either[Int, String]] =
      Task.fromTwitterFuture(
        Task(Cases.twitter.doFutureEither[Int, String]("twitter future either"))
      )
    val res22: IO[Int, String] = res2.mapError(_.getMessage.length).absolve
    show(res22)(runtime)
  }

  def withScalaFuture(): Unit = {
    println("ZIO with Scala Future: ")

    val res1: Task[String] = Task.fromFuture(_ => Cases.scala.doFuture[String]("scala future"))
    val res11: IO[Int, String] = res1.mapError(_.getMessage.length)
    show(res11)(runtime)

    val res2: Task[Either[Int, String]] =
      Task.fromFuture(_ => Cases.scala.doFutureEither[Int, String]("scala future either"))
    val res22: IO[Int, String] = res2.mapError(_.getMessage.length).absolve
    show(res22)(runtime)
  }

  def show[E, T](io: IO[E, T])(implicit runtime: Runtime[Unit]): Unit =
    println(" - " + runtime.unsafeRun(io))
}
