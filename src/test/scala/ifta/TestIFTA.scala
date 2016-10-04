/**
  * Created by jose on 30/09/16.
  */
package ifta

import org.scalatest.junit.JUnitSuite
import org.junit.Test
import org.junit.Assert._
import DSL._


class TestIFTA extends JUnitSuite {

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

  val cm2 = ifta +++ (
      0 --> 1 by "c",
      0 --> 1 by "ca",
      1 --> 0 by "b"
    ) startWith 0 get "c" get "ca" pub "b"


  val router =
    IFTA(
      Set(3,4),
      Set(3),
      Set("c","ca","i"),
      Set(),
      Set(),
      Set(
        Edge(3,true,Set("i"),Set(),true,4),
        Edge(4,true,Set("c"),Set(),true,3),
        Edge(4,true,Set("ca"),Set(),true,3)),
      Map(),
      FTrue,
      Set(),
      Set("i"),
      Set("c","ca"))

  val cmRouter = IFTA(Set(0, 1, 2, 3),Set(0),Set("b", "i"),Set(),Set(),Set(Edge(1,CTrue,Set("i"),Set(),FTrue,3), Edge(2,CAnd(CTrue,CTrue),Set("ca"),Set(),FAnd(FTrue,FTrue),1), Edge(1,CAnd(CTrue,CTrue),Set("b", "i"),Set(),FAnd(FTrue,FTrue),2), Edge(2,CAnd(CTrue,CTrue),Set("c"),Set(),FAnd(FTrue,FTrue),1), Edge(3,CTrue,Set("b"),Set(),FTrue,2), Edge(1,CTrue,Set("b"),Set(),FTrue,0), Edge(0,CTrue,Set("i"),Set(),FTrue,2)),Map(0 -> CAnd(CTrue,CTrue), 2 -> CAnd(CTrue,CTrue), 1 -> CAnd(CTrue,CTrue), 3 -> CAnd(CTrue,CTrue)),FAnd(FOr(FNot(FTrue),FTrue),FOr(FNot(FTrue),FTrue)),Set(),Set("i"),Set("b"))

//  @Test def TestComm(): Unit =
//    assertEquals("composition comutes",cm*router,router*cm)
  @Test def testDSL(): Unit =
    assertEquals(cm,cm2)

  @Test def testComp(): Unit =
    assertEquals(cm*router,cmRouter)

}
