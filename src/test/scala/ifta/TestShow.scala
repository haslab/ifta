package ifta

import ifta.analyse.Simplify
import ifta.backend.Show
import org.junit.Test
import org.junit.Assert._
import DSL._

/**
  * Created by jose on 03/10/16.
  */
class TestShow {
  val cm = newifta ++ (
    0 --> 1 by "co" when "cf"&&"mk" reset "c",
    0 --> 1 by "ca" when "cf"       reset "c",
    1 --> 0 by "b" cc "c">=2
    ) startWith 0 inv(1,"c"<=5) get "c" get "ca" pub "b" when "mk"-->"cf"

  @Test def testShow(): Unit = {
    // IFTA
    assertEquals(cm.toString,
      s"""IFTA [0|1] [co,ca,b] [c] [cf,mk] [c,ca]->[b]${" "}
         |  mk --> cf
         |  0 --> 1 by co reset c when cf && mk${" "}
         |  1 --> 0 by b cc c >= 2${" "}
         |  0 --> 1 by ca reset c when cf """.stripMargin)
    // FExp
    assertEquals(Show(("a" || "b") && FNot("a") && FNot("b")), "(a || b) && !a && !b" )
    // ClockCons
    assertEquals(Show("a" < 2 & "b" >= 3:ClockCons),"a < 2 & b >= 3")
  }

}
