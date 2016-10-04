package ifta

import ifta.DSL._

/**
  * Created by jose on 04/10/16.
  */
object Examples {

  val cm = ifta +++ (
    0 --> 1 by "co" when "cf"&&"mk" reset "c",
    0 --> 1 by "ca" when "cf"       reset "c",
    1 --> 0 by "b" cc "c">=2
    ) startWith 0 when "mk"-->"cf" inv(1,"c"<=5) get "co" get "ca" pub "b"


  val router = ifta +++ (
    3 --> 4 by "i"  when "vi" && ("vo1"||"vo2"),
    4 --> 3 by "co" when "vi" && "vo1",
    4 --> 3 by "ca" when "vi" && "vo2"
    ) startWith 3 get "i" pub "co" pub "ca"

}