package ifta

import ifta.DSL._

/**
  * Created by jose on 04/10/16.
  */
object Examples {

  val cm = newifta ++ (
    0 --> 1 by "ca" when "cf"&&"mk" reset "c",
    0 --> 1 by "co" when "cf"       reset "c",
    1 --> 0 by "b" cc "c">=2
    ) startWith 0 when "mk"-->"cf" inv(1,"c"<=5) get "co" get "ca" pub "b"

//  val routerCoCa = newifta ++ (
//    3 --> 4 by "i"  when "vi" && ("vo1"||"vo2"),
//    4 --> 3 by "co" when "vi" && "vo1",
//    4 --> 3 by "ca" when "vi" && "vo2"
//    ) startWith 3 get "i" pub "co" pub "ca"

  val routerCoCa = router("i","co","ca")

  val network = cm || routerCoCa

  ////////////////////
  // Reo connectors //
  ////////////////////
  def v(s:String) = "v"+s

  def router(i:String,o1:String,o2:String) = newifta ++ (
    0 --> 0 by s"$i,$o1" when v(i) && v(o1),
    0 --> 0 by s"$i,$o2" when v(i) && v(o2),
    0 --> 0 by s"$i" when (v(i) && not(v(o1) || v(o2))) // can receive if there are no outputs
    ) get i pub s"$o1,$o2" when (v(o1) || v(o2)) --> v(i)

  def fifo(i:String,o:String) = newifta ++ (
    0 --> 1 by s"$i" when v(i) && v(o),
    1 --> 0 by s"$o" when v(i) && v(o),
    0 --> 0 by s"$i" when v(i) && not(v(o))  // can receive if there is no output
    ) get i pub o   when v(o) --> v(i)

  def sync(i:String,o:String) = newifta ++ (
    0 --> 0 by s"$i,$o" when v(i) && v(o),
    0 --> 0 by s"$i" when v(i) && not(v(o))  // can receive if there is no output
//    0 --> 0 by s"$o" when not(v(i)) && v(o) // drop: cannot produce if there is no input
    ) get i pub o  when v(o) --> v(i)

  def sdrain(i:String,i2:String) = newifta ++ (
    0 --> 0 by s"$i,$i2" when v(i) && v(i2),
    0 --> 0 by s"$i" when v(i) && not(v(i2)),
    0 --> 0 by s"$i2" when v(i2) && not(v(i))
    ) get i get i2

  def asdrain(i:String,i2:String) = newifta ++ (
    0 --> 0 by s"$i" when v(i),
    0 --> 0 by s"$i2" when v(i2)
    ) get i get i2

  def join(i:String,i2:String,o:String) = newifta ++ (
    0 --> 0 by s"$i,$i2,$o" when v(i) && v(i2) && v(o),     // default: all ports
    0 --> 0 by s"$i2,$o" when not(v(i)) && v(i2) && v(o),   // can behave as sync if only one input and the output are present
    0 --> 0 by s"$i2" when not(v(i)) && v(i2) && not(v(o)), // can behave as asyncdrain if only one input is present
    0 --> 0 by s"$i,$i2" when v(i) && v(i2) && not(v(o)),   // can behave as syncdrain if only the inputs are present
    0 --> 0 by s"$i,$o" when v(i) && not(v(i2)) && v(o),    // can behave as sync if only one input and the output is present
    0 --> 0 by s"$i" when v(i) && not(v(i2)) && not(v(o)) // can behave as asyncdrain if only one input is present
    ) get i get i2 pub o  when v(o) --> (v(i) || v(i2))

  def merger(i:String, i2:String, o:String) = newifta ++ (
    0 --> 0 by s"$i,$o" when v(i) && v(o),    // default
    0 --> 0 by s"$i" when v(i) && not(v(o)),  // can behave as asyncdrain if there is no output
    0 --> 0 by s"$i2,$o" when v(i2) && v(o),  // default
    0 --> 0 by s"$i2" when v(i2) && not(v(o)) // can behave as asyncdrain if there is no output
    ) get i get i2 pub o  when v(o) --> (v(i) || v(i2))

  def repl(i:String,o1:String,o2:String) = newifta ++ (
    0 --> 0 by s"$i,$o1,$o2" when v(i) && v(o1) && v(o2), //default: all ports
    0 --> 0 by s"$i,$o1" when v(i) && v(o1),              // can behave as sync if only one output is present
    0 --> 0 by s"$i,$o2" when v(i) && v(o2),              // can behave as sync if only one output is present
    0 --> 0 by s"$i" when v(i) && not(v(o1) || v(o2))     // can behave as asyncdrain if only the input is present
    ) get i pub o1 pub o2  when (v(o1) || v(o2)) --> v(i)

  // other examples
  val router3 = (router("i","o1","o2") * router("i1","o3","o4")) sync(("o1","i1"))
  //  sequencer a>b>c :
  val sequencer3network = fifo("i","o") || repl("o","a","o2") || fifo("o2","o3") || repl("o3","b","o5") || fifo("o5","o6") || repl("o6","c","i")
  val sequencer3 = fifo("i","o") * repl("o","a","o2") * fifo("o2","o3") * repl("o3","b","o5") * fifo("o5","o6") * repl("o6","c","i")

  // examples:
  // repl("a","b","c") *  sync("b","d") *  fifo("c","e") // compose 3 connectors with product
  // repl("a","b","c") || sync("b","d") || fifo("c","e") // network of 3 connectors
  // note: last one works worse in UPPAAL: it is possible to reach deadlock by chosing the wrong sequence of committed
  //      locations (UPPAAL is not great with sequences of committed states).

}
