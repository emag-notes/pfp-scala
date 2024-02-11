package examples.newtypes

import io.estatico.newtype.macros._

object newts {
  @newtype case class Username(value: String)
  @newtype case class Email(value: String)

  val foo = Username("gvolpe")
//  val bar = foo.copy(value = "") // Does not compile

  def main(args: Array[String]): Unit = {
    println(foo)
//    println(bar)
  }
}
