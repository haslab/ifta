package ifta.examples

import ifta.DSL._

/**
  * Created by guille on 27/10/16.
  */
object ComplexConnectors {

  val router3 = (router("i","o1","o2") * router("i1","o3","o4")) sync(("o1","i1"))

  //  Sequencer a>b>c :
  val sequencer3network = (
    fifo("i","o") || repl("o","a","o2")
      || fifo("o2","o3") || repl("o3","b","o5")
      || fifo("o5","o6") || repl("o6","c","i")
    )

  val sequencer3 = (
    fifo("i","o") * repl("o","a","o2")
      * fifo("o2","o3") * repl("o3","b","o5")
      * fifo("o5","o6") * repl("o6","c","i")
    )

  // examples:
  // repl("a","b","c") *  sync("b","d") *  fifo("c","e") // compose 3 connectors with product
  // repl("a","b","c") || sync("b","d") || fifo("c","e") // network of 3 connectors
  // note: last one works worse in UPPAAL: it is possible to reach deadlock by chosing the wrong sequence of committed
  //      locations (UPPAAL is not great with sequences of committed states).

}
