package examples.newtypes

import eu.timepit.refined.api.Refined
import eu.timepit.refined.auto._
import eu.timepit.refined.collection.Contains

object refinement {
  type Username = String Refined Contains['g']

  def foo(username: Username): String = username.value

  def main(args: Array[String]): Unit = {
//    foo("gvolpe")
  }
}
