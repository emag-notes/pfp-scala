package examples.newtypes

import eu.timepit.refined.auto._
import eu.timepit.refined.types.string.NonEmptyString
import io.estatico.newtype.macros.newtype

object refinednewts {
  @newtype case class Brand(value: NonEmptyString)
  @newtype case class Category(value: NonEmptyString)

  def main(args: Array[String]): Unit = {
    val brand = Brand("foo")
    println(brand)
  }
}
