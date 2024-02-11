package examples.newtypes

import cats.effect.IO
import cats.implicits._
import examples.newtypes.valueclasses.Username

object valueclasses {
  case class User(username: Username, email: Email)

  def lookup(username: Username, email: Email): IO[Option[User]] =
    IO.pure(User(username, email).some)

  case class Username private (val value: String) extends AnyVal
  case class Email private (val value: String)    extends AnyVal

  def mkUsername(value: String): Option[Username] =
    if (value.nonEmpty) Username(value).some
    else none[Username]

  def mkEmail(value: String): Option[Email] =
    if (value.contains("@")) Email(value).some
    else none[Email]

  val foo =
    (
      mkUsername("aeinstein"),
      mkEmail("aeinstein@research.com")
    ).mapN { case (username, email) =>
      lookup(username, email)
    }

  val bar =
    (
      mkUsername("aeinstein"),
      mkEmail("aeinstein@research.com")
    ).mapN { case (username, email) =>
      lookup(username.copy(value = ""), email)
    }

  def main(args: Array[String]): Unit = {
    println(foo)
    println(bar)
  }
}
