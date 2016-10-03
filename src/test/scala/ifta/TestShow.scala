package ifta

import ifta.backend.Show
import org.junit.Test
import org.junit.Assert._
import DSL._

/**
  * Created by jose on 03/10/16.
  */
class TestShow {
  val cm = IFTA(
    Set(0,1),
    Set(0),
    Set("c","ca","b"),
    Set(),
    Set(),
    Set(
      Edge(0,true,Set("c"),Set(),true,1),
      Edge(0,true,Set("ca"),Set(),true,1),
      Edge(1,true,Set("b"),Set(),true,0)),
    Map(),
    FTrue,
    Set(),
    Set("c","ca"),
    Set("b"))

  @Test def testShow(): Unit = {
    // IFTA
    assertEquals(cm.toString,"IFTA(Set(0, 1),Set(0),Set(c, ca, b),Set(),Set(),Set(Edge(0,CTrue,Set(c),Set(),FTrue,1), Edge(0,CTrue,Set(ca),Set(),FTrue,1), Edge(1,CTrue,Set(b),Set(),FTrue,0)),Map(),FTrue,Set(),Set(c, ca),Set(b))")
    // FExp
    assertEquals(Show(("a" || "b") && FNot("a") && FNot("b")), "(a || b) && !a && !b" )
    // ClockCons
    assertEquals(Show(("a"<2 & "b">=3):ClockCons),"a < 2.0 & b >= 3.0")
  }

}
