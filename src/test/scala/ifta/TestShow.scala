package ifta

import ifta.backend.Show
import org.junit.Test
import org.junit.Assert._
import DSL._

/**
  * Created by jose on 03/10/16.
  */
class TestShow {
  val cm = ifta +++ (
    0 --> 1 by "co" when "cf"&&"mk" reset "c",
    0 --> 1 by "ca" when "cf"       reset "c",
    1 --> 0 by "b" cc "c">=2
    ) startWith 0 inv(1,"c"<=5) get "c" get "ca" pub "b"

  @Test def testShow(): Unit = {
    // IFTA
    assertEquals(cm.toString,"IFTA(Set(0, 1),Set(0),Set(co, ca, b),Set(c),Set(cf, mk),Set(Edge(0,CTrue,Set(co),Set(c),FAnd(FTrue,FAnd(Feat(cf),Feat(mk))),1), Edge(0,CTrue,Set(ca),Set(c),FAnd(FTrue,Feat(cf)),1), Edge(1,GE(c,2.0),Set(b),Set(),FTrue,0)),Map(1 -> LE(c,5.0)),FTrue,Set(),Set(c, ca),Set(b))")
    // FExp
    assertEquals(Show(("a" || "b") && FNot("a") && FNot("b")), "(a || b) && !a && !b" )
    // ClockCons
    assertEquals(Show(("a"<2 & "b">=3):ClockCons),"a < 2.0 & b >= 3.0")
  }

}
