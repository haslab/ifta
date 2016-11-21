package ifta.examples

import ifta.DSL._
import ifta.reo.FamConnectors._

/**
  * Created by guille on 27/10/16.
  */
object ComplexConnectors {

  ///////////////
  // Sequencer //
  ///////////////

  //  Sequencer a>b>c as a NIFTA: todo: adapt for new connectors
  val seq3net =
    fifofull("i","o") || repl("o","a","o2") ||
      fifo("o2","o3") || repl("o3","b","o5") ||
      fifo("o5","o6") || repl("o6","c","i")

  // Sequencer a>b>c as an IFTA
  val seq3 = seq3net.flatten

  /////////////////////
  // Multiple Merger //
  /////////////////////

//  // Multiple merger for two components C1 and C2:
  lazy val multipleMergerNet =
    repl3 || fifo3 || fifo4 || merger1 || merger2 ||
    C1 || C2 || repl2 || repl4 || router1 ||
    fifo2 || fifo5 || router2 || sdrain2 || sdrain3 ||
    router3 || sdrain4 || sdrain5 ||
    repl6 || repl7 || repl5 || repl8 || fifo6 || fifo7 ||
    sdrain6 || repl9 || merger4 || merger3 ||
    fifo1 || repl1 || sdrain1 //when (("v_c1outc2out") <-> ("C1" || "C2")) // &&
//      ("vc1inc2in" <-> "vc1outc2out"))


  val repl1 = repl("c1inc2in","repl1o1","repl1o2") //.relax when "v_c1inc2in" <-> "C1" || "C2"

  val router1 = router("repl1o1","router1o1","router1o2","router1o3").relax when (
    (("v_router1o1" ||"v_router1o2"|| "v_router1o3") --> "v_repl1o1")  && (("C1" && "C2") <-> "v_router1o2"))

  val fifo1 = fifo("repl1o2","fifo1o")

  val repl2 = repl("router1o1","repl2o1","repl2o2")

  val repl3 = repl("router1o2","repl3o1", "repl3o2","repl3o3","repl3o4")

  val repl4 = repl("router1o3","repl4o1","repl4o2")

  val sdrain1 = sdrain("fifo1o","sdrain1i2")

  val fifo2 = fifo("repl2o1","fifo2o")

  val merger1 = merger("repl2o2","repl3o1","c1in").relax when ("v_c1in" <-> "v_repl2o2") && ("v_repl3o1" --> "v_c1in")

  val fifo3 = fifo("repl3o2","fifo3o")

  val fifo4 = fifo("repl3o3","fifo4o")

  val merger2 = merger("repl3o4","repl4o1","c2in").relax when ("v_c2in" <-> "v_repl4o1") && ("v_repl3o4" --> "v_c2in")

  val fifo5 = fifo("repl4o2","fifo5o")

  val repl5 = repl("fifo2o","repl5o1","repl5o2","repl5o3")

  val sdrain2 = sdrain("repl5o3","router2o1")

  val router2 = router("c1out","router2o1","router2o2").relax when ("v_c1out" <-> "v_router2o1") && ("v_router2o2" --> "v_c1out")

  val sdrain3 = sdrain("router2o2","repl6o1")

  val repl6 = repl("fifo3o","repl6o1","repl6o2","repl6o3")

  val repl7 = repl("fifo4o","repl7o1","repl7o2","repl7o3")

  val sdrain4 = sdrain("repl7o3","router3o1")

  val router3 = router("c2out","router3o1","router3o2").relax when ("v_c2out" <-> "v_router3o2") && ("v_router3o1" --> "v_c2out")

  val sdrain5 = sdrain("router3o2","repl8o1")

  val repl8 = repl("fifo5o","repl8o1","repl8o2","repl8o3")

  val fifo6 = fifo("repl6o2","fifo6o")

  val fifo7 = fifo("repl7o2","fifo7o")

  val sdrain6 = sdrain("fifo6o","repl9o1")

  val repl9 = repl("fifo7o","repl9o1","repl9o2")

  val merger3 = merger("repl5o1","repl6o3","repl7o1","repl8o2","c1outc2out").relax when
    (("v_repl5o1" || "v_repl6o3" || "v_repl7o1" || "v_repl8o2" ) <-> "v_c1outc2out") //&&
//    ("v_repl5o1" <-> "C1") &&
//    (("v_repl6o3" || "v_repl7o1") <-> ("C1" && "C2")) &&
//    ("v_repl8o2" <-> "C2")


  val merger4 = merger("repl9o2","repl5o2","repl8o3","sdrain1i2").relax when
    (("v_repl9o2" || "v_repl5o2" || "v_repl8o3") <-> "v_sdrain1i2")// &&
//    ("v_repl5o2" <-> "C1") &&
//    ("v_repl8o3" <-> "C2") &&
//    ("v_repl9o2" <-> ("C1"&&"C2"))
//    )

  // Component C1
  val C1 = newifta ++ (
    0 --> 1 by "c1in" when "C1",
    1 --> 0 by "c1out" when "C1"
    ) startWith 0 get "c1in" pub "c1out"

  // Component C2
  val C2 = newifta ++ (
    0 --> 1 by "c2in" when "C2",
    1 --> 0 by "c2out" when "C2"
    ) startWith 0 get "c2in" pub "c2out"



  // Test Connectors

  merger("i1","i2","o") //fixed
  merger("i1","i2","o").relax // variable where the only restriction for each edge is that the fe of each oction is present.
  merger("i1","i2","o").relax  exclusive "o" // variable restricting each edge e (where "o" not in e.act) to execute only if "v_o" is not present

  join("i1","i2","o")
  join("i1","i2","o").relax
  join("i1","i2","o").relax  exclusive  "i1,i2,o"

  sync("i","o")
  sync("i","o").relax
  sync("i","o").relax exclusive "o"

  fifo("i","o")
  fifo("i","o").relax
  // doesn't work currently: possible solution remove loop

  repl("i","o1","o2")
  repl("i","o1","o2").relax
  repl("i","o1","o2").relax exclusive "o1,o2"

  router("i","o1","o2")
  router("i","o1","o2").relax
  router("i","o1","o2").relax exclusive "i" -> "o1,o2" //variable where only edge with action Set("i") is restricted to execute only if not(v_o1 && v_o2)




//  val wrapC1 = merger1 || C1 || router2 when "C1"
//
//  val wrapC2 = merger2 || C2 || router3 when "C2"
//
//  val callC1 = repl2 || fifo2  when "C1"
//
//  val callBoth = repl3 || fifo3 || fifo4 when "C1" && "C2"
//
//  val callC2 = repl4 || fifo5 when "C2"
//
//  val waitC1 = repl5 || sdrain2 when "C1"
//
//  val waitC2 = repl8 || sdrain5 when "C2"
//
//  val waitBoth = repl6 || sdrain3 || fifo6 || repl7 || sdrain4 || fifo7 || repl9 || sdrain6 when "C1" && "C2"
//
//  val busy = fifo1 || sdrain1 when "C1" || "C2"
//
//  val release = merger4 when "C1" || "C2"
//
//  val mergeAnswers = merger3 when "C1" || "C2"
//
//  val makeChoice = router1 when "C1" || "C2"
//
//  val call = repl1 || makeChoice when "C1" || "C2"
//
//  val multMergerNet = (call || busy || callC1 || callC2 || callBoth || wrapC1 || wrapC2 ||
//    waitC1 || waitC2 || waitBoth || release || mergeAnswers) when (
//    (("vc1inc2in" && "vc1outc2out") <-> ("C1" || "C2")) &&
//      ("vc1inc2in" <-> "vc1outc2out") &&
//      ("vrepl1o2" <-> ("C1" || "C2")) &&
//      ("vsdrain1i2" <-> ("C1" || "C2")) &&
//      ("vrouter1o2" <-> ("C1" && "C2")) &&
//      ("vrouter1o1" <-> "C1" ) &&
//      ("vrouter1o3" <-> "C2" ) &&
//      (("vrepl3o1" && "vrepl3o4" && "vfifo3o" && "vfifo4o") <-> ("C1" && "C2")) &&
//      (("vrepl2o2" && "vfifo2o") <-> "C1") &&
//      (("vrepl4o1" && "vfifo5o") <-> "C2") &&
//      (("vrouter2o2" && "vrouter2o1") <-> "C1") &&
//      (("vrouter3o2" && "vrouter3o1") <-> "C2") &&
//      (("vrepl7o1" && "vrepl6o3" && "vrepl9o2") <-> ("C1" && "C2")) &&
//      (("vrepl5o1" && "vrepl5o2") <-> "C1") &&
//      (("vrepl8o2" && "repl8o3") <-> "C2"))



  // examples:
  // repl("a","b","c") *  sync("b","d") *  fifo("c","e") // compose 3 connectors with product
  // repl("a","b","c") || sync("b","d") || fifo("c","e") // network of 3 connectors
  // note: last one works worse in UPPAAL: it is possible to reach deadlock by chosing the wrong sequence of committed
  //      locations (UPPAAL is not great with sequences of committed states).

}
