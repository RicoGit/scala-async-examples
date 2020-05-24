package union

sealed trait Result[E, T] { self =>
  import Result._

  def flatMap[TT >: T](fn: T => Result[E, TT]): Result[E, TT] = {
    self match {
      case Ok(value)    => fn(value)
      case err @ Err(_) => err.as[TT]
    }
  }

  def map[TT >: T](fn: T => TT): Result[E, TT] = {
    self match {
      case Ok(value)    => fn(value).ok[E]
      case err @ Err(_) => err.as[TT]
    }
  }

}

object Result {

  implicit class ResultOps[X](value: X) {
    def ok[E]: Result[E, X] = Ok(value)
    def err[T]: Result[X, T] = Err(value)
  }

  case class Ok[E, T](value: T) extends Result[E, T] {
    def as[EE >: T]: Result[EE, T] = Ok(value)
  }

  case class Err[E, T](value: E) extends Result[E, T] {
    def as[TT >: T]: Result[E, TT] = Err(value)
  }

}

object Test {
  import Result._

  def test(): Result[Int, String] = {
    val res: Result[Int, String] = for {
      ok1 <- "test1".ok[Int]
      ok2 <- "test2".ok[Int]
      err <- 42.err[String]
    } yield ok1 + " " + ok2

    res
  }

}
