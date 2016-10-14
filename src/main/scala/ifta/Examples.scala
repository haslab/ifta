package ifta

import ifta.DSL._

/**
  * Created by jose on 04/10/16.
  */
object Examples {

  val cm = newifta ++ (
    0 --> 1 by "co" when "cf"&&"mk" reset "c",
    0 --> 1 by "ca" when "cf"       reset "c",
    1 --> 0 by "b" cc "c">=2
    ) startWith 0 when "mk"-->"cf" inv(1,"c"<=5) get "co" get "ca" pub "b"


  val routerCoCa = newifta ++ (
    3 --> 4 by "i"  when "vi" && ("vo1"||"vo2"),
    4 --> 3 by "co" when "vi" && "vo1",
    4 --> 3 by "ca" when "vi" && "vo2"
    ) startWith 3 get "i" pub "co" pub "ca"

//  val network = nifta <<>> (cm,router)
  val network = cm || routerCoCa


  ////////////////////
  // Reo connectors //
  ////////////////////
  def v(s:String) = "v"+s

  def router(i:String,o1:String,o2:String) = newifta ++ (
    0 --> 0 by s"$i,$o1" when v(i) && v(o1),
    0 --> 0 by s"$i,$o2" when v(i) && v(o2)
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

  def repl(i:String,o1:String,o2:String) = newifta ++ (
    0 --> 0 by s"$i,$o1,$o2" when v(i) && v(o1) && v(o2),
    0 --> 0 by s"$i,$o1" when v(i) && v(o1),
    0 --> 0 by s"$i,$o2" when v(i) && v(o2)
    ) get i pub o1 pub o2  when (v(o1) || v(o2)) --> v(i)

  // examples:
  // repl("a","b","c") *  sync("b","d") *  fifo("c","e") // compose 3 connectors with product
  // repl("a","b","c") || sync("b","d") || fifo("c","e") // network of 3 connectors
  // note: last one works worse in UPPAAL: it is possible to reach deadlock by chosing the wrong sequence of committed
  //      locations (UPPAAL is not great with sequences of committed states).
}
