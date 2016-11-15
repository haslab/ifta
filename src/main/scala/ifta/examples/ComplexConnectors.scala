package ifta.examples

import ifta.DSL._
import ifta.reo.Connectors._

/**
  * Created by guille on 27/10/16.
  */
object ComplexConnectors {

  //  Sequencer a>b>c :
  val seq3net =
    fifofull("i","o") || repl("o","a","o2") ||
      fifo("o2","o3") || repl("o3","b","o5") ||
      fifo("o5","o6") || repl("o6","c","i")


  // Composed IFTA
  val seq3 = seq3net.flatten

  // examples:
  // repl("a","b","c") *  sync("b","d") *  fifo("c","e") // compose 3 connectors with product
  // repl("a","b","c") || sync("b","d") || fifo("c","e") // network of 3 connectors
  // note: last one works worse in UPPAAL: it is possible to reach deadlock by chosing the wrong sequence of committed
  //      locations (UPPAAL is not great with sequences of committed states).

}
