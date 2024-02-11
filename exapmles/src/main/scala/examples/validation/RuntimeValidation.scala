package examples.validation

import cats.data.{ EitherNel, ValidatedNel }
import cats.effect._
import cats.implicits._
import eu.timepit.refined._
import eu.timepit.refined.api.RefType.refinedRefType
import eu.timepit.refined.api._
import eu.timepit.refined.auto._
import eu.timepit.refined.collection.Contains
import eu.timepit.refined.numeric.Greater
import eu.timepit.refined.types.string.NonEmptyString
import io.estatico.newtype.macros._

import scala.util.control.NoStackTrace

object RuntimeValidation extends IOApp.Simple {
  override def run: IO[Unit] = p12("a", "b", "c")
  // basic
  def showName(username: String, name: String, email: String): String =
    s"Hi $name! Your username is $username and your email is $email."
  val p0: IO[Unit] = IO.println(showName("gvolpe@github.com", "12345", "foo"))
  // Value classes
  final case class UserNameV(value: String) extends AnyVal
  final case class NameV(value: String)     extends AnyVal
  final case class EmailV(value: String)    extends AnyVal
  val badUesrName = UserNameV("gvolpe@gihtub.com")
  def showNameV(username: UserNameV, name: NameV, email: EmailV): String =
    s"Hi ${name.value}! Your username is ${username.value} and your email is ${email.value}."
  val p1: IO[Unit] = IO.println(showNameV(badUesrName.copy(value = ""), NameV("12345"), EmailV("foo")))
  // Sealed abstract case classes
  sealed abstract case class UserNameP(value: String)
  object UserNameP {
    def apply(value: String): Option[UserNameP] =
      value.nonEmpty.guard[Option].as(new UserNameP(value) {})
  }
  sealed abstract case class NameP(value: String)
  object NameP {
    def apply(value: String): Option[NameP] =
      value.nonEmpty.guard[Option].as(new NameP(value) {})
  }
  sealed abstract case class EmailP(value: String)
  object EmailP {
    def apply(value: String): Option[EmailP] =
      value.contains("@").guard[Option].as(new EmailP(value) {})
  }
  def showNameP(username: UserNameP, name: NameP, email: EmailP): String = {
    s"Hi ${name.value}! Your username is ${username.value} and your email is ${email.value}."
  }
  val p2: IO[Unit] =
    (
      UserNameP("jr"),
      NameP("Joe Reef"),
      EmailP("Joe@bar.com")
    ).traverseN { case (u, n, e) =>
      IO.println(showNameP(u, n, e))
    }.void
  // Newtypes
  @newtype case class UserNameT(value: String)
  @newtype case class NameT(value: String)
  @newtype case class EmailT(value: String)
  def showNameT(username: UserNameT, name: NameT, email: EmailT): String =
    s"Hi ${name.value}! Your username is ${username.value} and your email is ${email.value}."
  val p3: IO[Unit] = IO.println(showNameT(UserNameT("jr"), NameT("Joe Reef"), EmailT("")))
  // Smart Constructors
  def mkUesrname(value: String): Option[UserNameT] =
    value.nonEmpty.guard[Option].as(UserNameT(value))
  def mkName(value: String): Option[NameT] =
    value.nonEmpty.guard[Option].as(NameT(value))
  def mkEmail(value: String): Option[EmailT] =
    value.contains("@").guard[Option].as(EmailT(value))
  case object EmptyError   extends NoStackTrace
  case object InvalidEmail extends NoStackTrace
  val p4: IO[Unit] =
    (
      mkUesrname("gvolpe").liftTo[IO](EmptyError),
      mkName("George").liftTo[IO](EmptyError),
      mkEmail("123").liftTo[IO](InvalidEmail)
    ).parMapN(showNameT)
      .flatMap(IO.println)
  // Refinement types
  type UserNameR = NonEmptyString
  object UserNameR extends RefinedTypeOps[UserNameR, String]
  type NameR = NonEmptyString
  object NameR extends RefinedTypeOps[NameR, String]
  type EmailR = String Refined Contains['@']
  object EmailR extends RefinedTypeOps[EmailR, String]
  def showNameR(username: UserNameR, name: NameR, email: EmailR): String =
    s"Hi ${name.value}! Your username is ${username.value} and your email is ${email.value}."
  val p5: IO[Unit] =
    IO.println(showNameR("jr", "Joe", "jr@gmail.com"))
  // Newtypes + Refined
  @newtype case class UserName(value: UserNameR)
  @newtype case class Name(value: NameR)
  @newtype case class Email(value: EmailR)
  def showNameTR(username: UserName, name: Name, email: Email): String =
    s"Hi ${name.value.value}! Your username is ${username.value.value} and your email is ${email.value.value}."
  val p6: IO[Unit] =
    IO.println(showNameTR(UserName("jr"), Name("Joe"), Email("foo@bar.com")))
  object NewTypeRefinedOps {
    import io.estatico.newtype.Coercible
    import io.estatico.newtype.ops._
    final class NewtypePartiallyApplied[A, T](raw: T) {
      def validate[P](implicit
          c: Coercible[Refined[T, P], A],
          v: Validate[T, P]
      ): EitherNel[String, A] =
        refineV[P](raw).toEitherNel.map(_.coerce[A])
    }
    implicit class NewtypeOps[T](raw: T) {
      def as[A]: NewtypePartiallyApplied[A, T] = new NewtypePartiallyApplied[A, T](raw)
    }
    final class NewtypeRefinedPartiallyApplied[A] {
      def apply[T, P](raw: T)(implicit
          c: Coercible[Refined[T, P], A],
          v: Validate[T, P]
      ): EitherNel[String, A] =
        refineV[P](raw).toEitherNel.map(_.coerce[A])
    }
    def validate[A]: NewtypeRefinedPartiallyApplied[A] = new NewtypeRefinedPartiallyApplied[A]
  }
  def c6(u: String, n: String, e: String): IO[Unit] = {
    import NewTypeRefinedOps._
    val result = (
      validate[UserName](u),
      validate[Name](n),
      validate[Email](e)
    ).parMapN(showNameTR)
    IO.println(result)
  }
  // Auto unwrapping
  object AutoUnrwapping {
    import eu.timepit.refined.types.numeric.PosInt
    @newsubtype(optimizeOps = false) case class Numer(value: PosInt)
    val n1       = Numer(87)
    val raw: Int = n1
  }
  val p7: IO[Unit] =
    IO.println(">>>>>>>> Unwrapping Newtype <<<<<<<<") >> IO.println(AutoUnrwapping.raw)
  // Refined + Validated
  type GTFive = Int Refined Greater[5]
  object GTFive extends RefinedTypeOps[GTFive, Int]
  case class MyType(a: NonEmptyString, b: GTFive)
  def p8(a: String, b: Int): IO[Unit] = {
    val result: Either[String, MyType] =
      for {
        x <- NonEmptyString.from(a)
        y <- GTFive.from(b)
      } yield MyType(x, y)
    IO.println(result)
  }
  def p9(a: String, b: Int): IO[Unit] = {
    val result: EitherNel[String, MyType] =
      (NonEmptyString.from(a).toEitherNel, GTFive.from(b).toEitherNel)
        .parMapN(MyType)
    IO.println(result)
  }
  def p10(a: String, b: Int): IO[Unit] = {
    val result: ValidatedNel[String, MyType] =
      (NonEmptyString.from(a).toValidatedNel, GTFive.from(b).toValidatedNel)
        .mapN(MyType)
    IO.println(result)
  }
  case class Person(
      username: UserName,
      name: Name,
      email: Email
  )
  def p11(u: String, n: String, e: String): IO[Unit] = {
    val result: EitherNel[String, Person] =
      (
        UserNameR.from(u).toEitherNel.map(UserName.apply),
        NameR.from(n).toEitherNel.map(Name.apply),
        EmailR.from(e).toEitherNel.map(Email.apply)
      ).parMapN(Person)
    IO.println(result)
  }
  def p12(u: String, n: String, e: String): IO[Unit] = {
    import NewTypeRefinedOps._
    val result: EitherNel[String, Person] =
      (
        u.as[UserName].validate,
        n.as[Name].validate,
        e.as[Email].validate
      ).parMapN(Person)
    IO.println(result)
  }
}
