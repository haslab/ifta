package ifta.analyse

import ifta.{FAnd, Feat, FExp}

/**
  * Created by jose on 03/10/16.
  * Uses SAT4J to check if a FExp is valid.
  */
object Solver {
  def apply(e:FExp): Boolean = true

  def main(args: Array[String]) {
    val exp = FAnd(Feat("a"),Feat("b"))
  }

}
