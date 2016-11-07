package ifta.examples

import ifta.DSL._

/**
  * Created by jose on 04/10/16.
  */
object CoffeeMachine {

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


  // now without shared names

  val myRouter = router("i","o1","o2")
  val link = List("o1"->"ca","o2"->"co")
  val net2 = (cm || myRouter) sync link
  // alternative (and equivalent) ways to calculate the product
  val aut2a = (cm sync link) * (myRouter sync link)
  val aut2b = (cm || myRouter) product link
  val aut2c = net2 product ()
}
