package interoperabillity

import monix.eval.Task
import monix.execution.Scheduler
import zio.{BootstrapRuntime, Task => ZTask}

/** Interop between Monix and other stuff. **/
object MonixInterop {

  def withFinalTagless(): Unit = {
    println("Monix with cats tagless final: ")

    import monix.execution.Scheduler.Implicits.global

    val res1: Task[String] = Cases.taglessFinal.doAsync[Task, String]("async")
    val res11: Task[Either[Int, String]] = res1.attempt.map(_.left.map(_.getMessage.length))
    show(res11)

    val res2: Task[Either[Int, String]] =
      Cases.taglessFinal.doAsyncEither[Task, Int, String]("async either")
    show(res2)

    {
      val res3: Task[Either[Int, String]] =
        Cases.taglessFinal.doEffect[Task, Int, String]("effect")
      show(res3)
    }
  }

  def WithCatsIO(): Unit = {
    println("Monix with Cats IO: ")

    import monix.execution.Scheduler.Implicits.global

    val res1: Task[String] = Cases.catsIO.doIo[String]("monix task").to[Task]
    val res11 = res1.attempt.map(_.left.map(_.getMessage.length))
    show(res11)

    val res2: Task[Either[Int, String]] =
      Cases.catsIO.doIoEither[Int, String]("monix task either").to[Task]
    show(res2)

  }

  def withTwitterFuture(): Unit = {
    println("Monix with Twitter Future: ")

    import io.catbird.util.effect.futureToAsync
    import monix.execution.Scheduler.Implicits.global

    val res1: Task[String] =
      futureToAsync[Task, String](Cases.twitter.doFuture[String]("twitter future"))
    val res11 = res1.attempt.map(_.left.map(_.getMessage.length))
    show(res11)

    val res2: Task[Either[Int, String]] =
      futureToAsync[Task, Either[Int, String]](
        Cases.twitter.doFutureEither[Int, String]("twitter future either")
      )
    show(res2)
  }

  def withScalaFuture(): Unit = {
    println("Monix with Scala Future: ")

    import monix.execution.Scheduler.Implicits.global

    val res1: Task[String] =
      Task.fromFuture(Cases.scala.doFuture[String]("scala future"))
    val res11 = res1.attempt.map(_.left.map(_.getMessage.length))
    show(res11)

    val res2: Task[Either[Int, String]] =
      Task.fromFuture(Cases.scala.doFutureEither[Int, String]("scala future either"))
    show(res2)
  }

  def withZio(): Unit = {
    println("Monix with ZIO: ")

    import monix.execution.Scheduler.Implicits.global
    import zio.interop.monix._
    implicit val runtime: BootstrapRuntime = new BootstrapRuntime {}

    val res1: ZTask[Either[RuntimeException, String]] =
      Cases.zio.doZio[RuntimeException, String]("zio").either
    val res11: Task[Either[RuntimeException, String]] = runtime.unsafeRun(res1.toTask)
    show(res11)
  }

  def show[T](task: Task[T])(implicit sc: Scheduler): Unit =
    println(" - " + task.runSyncUnsafe())
}
