package ifta.examples

import ifta.DSL._
//import ifta.examples.FamConnectors._

import ifta.analyse.Simplify
import ifta._

/**
  * Created by guille on 17/11/16.
  */
object ComposedConnectors {

  /////////////////////
  // Multiple Merger //
  /////////////////////


//  val repl1 = repl("c1inc2in","repl1o1","repl1o2")
//
//  val router1 = router("repl1o1","router1o1","router1o2","router1o3")
//
//  val fifo1 = fifo("repl1o2","fifo1o")
//
//  val repl2 = repl("router1o1","repl2o1","repl2o2")
//
//  val repl3 = repl("router1o2","repl3o1", "repl3o2","repl3o3","repl3o4")
//
//  val repl4 = repl("router1o3","repl4o1","repl4o2")
//
//  val sdrain1 = sdrain("fifo1o","sdrain1i2")
//
//  val fifo2 = fifo("repl2o1","fifo2o")
//
//  val merger1 = merger("repl2o2","repl3o1","c1in") //.relax when "c1in" --> "repl2o2" || "repl3o1" exclude Set("repl2o2","repl3o1")
//
//  val fifo3 = fifo("repl3o2","fifo3o")
//
//  val fifo4 = fifo("repl3o3","fifo4o")
//
//  val merger2 = merger("repl3o4","repl4o1","c2in")
//
//  val fifo5 = fifo("repl4o2","fifo5o")
//
//  val repl5 = repl("fifo2o","repl5o1","repl5o2","repl5o3")
//
//  val sdrain2 = sdrain("repl5o3","router2o1")
//
//  val router2 = router("c1out","router2o1","router2o2")
//
//  val sdrain3 = sdrain("router2o2","repl6o1")
//
//  val repl6 = repl("fifo3o","repl6o1","repl6o2","repl6o3")
//
//  val repl7 = repl("fifo4o","repl7o1","repl7o2","repl7o3")
//
//  val sdrain4 = sdrain("repl7o3","router3o1")
//
//  val router3 = router("c2out","router3o1","router3o2")
//
//  val sdrain5 = sdrain("router3o2","repl8o1")
//
//  val repl8 = repl("fifo5o","repl8o1","repl8o2","repl8o3")
//
//  val fifo6 = fifo("repl6o2","fifo6o")
//
//  val fifo7 = fifo("repl7o2","fifo7o")
//
//  val sdrain6 = sdrain("fifo6o","repl9o1")
//
//  val repl9 = repl("fifo7o","repl9o1","repl9o2")
//
//  val merger3 = merger("repl5o1","repl6o3","repl7o1","repl8o2","c1outc2out")
//
//  val merger4 = merger("repl9o2","repl5o2","repl8o3","sdrain1i2")
//
//  // Component C1
//  val C1 = newifta ++ (
//    0 --> 1 by "c1in" when "C1",
//    1 --> 0 by "c1out" when "C1"
//    ) startWith 0 get "c1in" pub "c1out"
//
//  // Component C2
//  val C2 = newifta ++ (
//    0 --> 1 by "c2in" when "C2",
//    1 --> 0 by "c2out" when "C2"
//    ) startWith 0 get "c2in" pub "c2out"
//
//  //////////////////////////////////////////
//  // Using riged and variable connectors: //
//  //////////////////////////////////////////
//
//  val multMergerNet = (repl3 || fifo3 || fifo4 || merger1 || merger2 ||
//      C1 || C2 || repl2 || repl4 || router1 ||
//      fifo2 || fifo5 || router2 || sdrain2 || sdrain3 ||
//      router3 || sdrain4 || sdrain5 ||
//      repl6 || repl7 || repl5 || repl8 || fifo6 || fifo7 ||
//      sdrain6 || repl9 || merger4 || merger3 ||
//      fifo1 || repl1 || sdrain1) when ( "C1" && "C2" && "v_c1inc2in" && "v_c1outc2out")
////      (("v_c1inc2in" && "v_c1outc2out") <-> ("C1" || "C2")))

}



/**
  * Created by jose on 17/11/16.
  * Each connector, e.g., router(...), is rigid: it has all ends or none.
  * To make it variable, one can apply a "vary" clause to relate ends.
  * E.g.,
  *   `sync("in","out").relax` means ports can be present in any combination (one variable per port),
  *   `sync("in","out").relax when "out" --> "in"` means out can appear only if "in" also does,
  *   `sync("in","out").relax exclusive "in"` means "in" can only go if none of the other ports can go
  *   `router("i","o1","o2).relax when "o1"||"o2"-->"i" exclusive "in"`
  */
object FamAltConnectors {

  /** Wrap for IFTAs extended with `relax` and `exclusive` methods. */
  class Conn(locs: Set[Int], init: Int, act: Set[String], clocks: Set[String]
             , feats: Set[String], edges: Set[Edge], cInv: Map[Int, ClockCons], fm: FExp
             , in: Set[String], out: Set[String], aps: Map[Int, String], shortname: String = "")
    extends IFTA(locs, init, act, clocks, feats, edges, cInv, fm, in, out, aps, shortname) {
    def relax: Conn =
      new Conn(locs, init, act, clocks, act.map(v), edges.map(relaxEdge), cInv, fm, in, out, aps, shortname)

    private def relaxEdge(e: Edge): Edge =
      Edge(e.from, e.cCons, e.act, e.cReset, e.act.foldRight[FExp](FTrue)((s, fs) => v(s) && fs), e.to)

    override def when(f: FExp): Conn =
      new Conn(locs, init, act, clocks, feats ++ f.feats, edges, cInv, fm && f, in, out, aps, shortname)

    def exclude(p: String): Conn = exclude(Set(p))

    def exclude(ps: Set[String]): Conn =
      new Conn(locs, init, act, clocks, act, edges.map(excludeEdge(ps, act)), cInv, fm, in, out, aps, shortname)

    private def excludeEdge(ps: Set[String], as: Set[String])(e: Edge): Edge =
      if (ps != e.act) e
      else Edge(e.from, e.cCons, e.act, e.cReset,
        e.act.foldRight[FExp](FTrue)((s, fs) => v(s) && fs) &&
          (as -- e.act).foldRight[FExp](FTrue)((s, fs) => not(v(s)) && fs),
        e.to)
  }

  // auxilary constructions
  private def v(s: String) = "v_" + s

  private def mkFeat(act: Set[String]): Set[FExp] = act.map(a => Feat(v(a)))

  private def mkFAnd(feats: Set[FExp]): FExp = Simplify(feats.fold(FTrue)(_ && _))

  private def mkFOr(feats: Set[FExp]): FExp = Simplify(feats.fold(FNot(FTrue))(_ || _))

  private def mkConn(i: IFTA) =
    new Conn(i.locs, i.init, i.act, i.clocks, i.feats, i.edges, i.cInv, i.fm, i.in, i.out, i.aps, i.shortname)


//  def fifo(i: String, o: String) = {
//    val feat = v(s"${i}_${o}")
//    mkConn(newifta ++ (
//      0 --> 1 by s"$i" when feat,
//      1 --> 0 by s"$o" when feat,
//      0 --> 0 by s"$i" when FNot(FTrue)
//      ) get i pub o name "[]")
//  }
//
//  def fifofull(i: String, o: String) = {
//    val feat = v(s"${i}_${o}")
//    mkConn(newifta ++ (
//      0 --> 1 by s"$o" when feat,
//      1 --> 0 by s"$i" when feat
//      ) get i pub o name "[.]")
//  }
//
//  def sync(i: String, o: String) = {
//    val feat = v(s"${i}_${o}")
//    mkConn(newifta ++ (
//      0 --> 0 by s"$i,$o" when feat,
//      0 --> 0 by s"$i" when FNot(FTrue)
//      ) get i pub o name "->")
//  }
//
//  def sdrain(i: String, i2: String) = {
//    val feat = v(s"${i}_${i2}")
//    mkConn(newifta ++ (
//      0 --> 0 by s"$i,$i2" when feat,
//      0 --> 0 by s"$i" when FNot(FTrue),
//      0 --> 0 by s"$i2" when FNot(FTrue)
//      ) get i get i2 name ">-<")
//  }
//
//  def asdrain(i: String, i2: String) = {
//    val feat = v(s"${i}_${i2}")
//    mkConn(newifta ++ (
//      0 --> 0 by s"$i" when feat,
//      0 --> 0 by s"$i2" when feat
//      ) get i get i2 name ">|<")
//  }
//
//  def router(i: String, o1: String, o2: String, outs: String*) = {
//    val feat = v(s"${i}_${o1}_$o2" + (if (outs.isEmpty) "" else outs.mkString("_", "_", "")))
//    val nouts = Set(o1, o2) ++ outs.toSet
//    var ifta = newifta ++ (0 --> 0 by i when FNot(FTrue))
//    for (o <- nouts)
//      ifta ++= (0 --> 0 by Set(i, o) when feat)
//    mkConn(ifta get i pub nouts.mkString(",") name "Xor")
//  }
//
//  def join(i: String, i2: String, inOrO: String, rest: String*) = {
//    val feat = v(s"${i}_${i2}_$inOrO" + (if (rest.isEmpty) "" else rest.mkString("_", "_", "")))
//    val o = if (rest.isEmpty) inOrO else rest.last
//    val ins = if (rest.nonEmpty) Set(i, i2, inOrO) ++ (rest.toSet - o) else Set(i, i2)
//    var ifta = newifta ++ (0 --> 0 by Set(o)++ins when feat)
//    val ss = (ins + o).subsets().toSet - Set() - Set(o)
//    for (s <- ss - (Set(o)++ins))
//      ifta ++= (0 --> 0 by s when FNot(FTrue))
//    mkConn(ifta get ins.mkString(",") pub o name "+")
//  }
//
//  def merger(i: String, i2: String, inOrO: String, rest: String*) = {
//    val feat = v(s"${i}_${i2}_$inOrO" + (if (rest.isEmpty) "" else rest.mkString("_", "_", "")))
//    val o = if (rest.isEmpty) inOrO else rest.last
//    var ifta = newifta
//    val ins = if (rest.nonEmpty) Set(i, i2, inOrO) ++ (rest.toSet - o) else Set(i, i2)
//    for (i <- ins)
//      ifta ++= (
//        0 --> 0 by Set(i, o) when feat,
//        0 --> 0 by Set(i) when FNot(FTrue)
//        )
//    mkConn(ifta get ins.mkString(",") pub o when v(o) --> mkFOr(mkFeat(ins)) name ">-")
//  }
//
//  def repl(i: String, o1: String, o2: String, outs: String*) = {
//    val feat = v(s"${i}_${o1}_$o2" + (if (outs.isEmpty) "" else outs.mkString("_", "_", "")))
//    val nouts = Set(o1, o2) ++ outs.toSet
//    var ifta = newifta ++ (0 --> 0 by Set(i)++nouts when feat)
//    for (ss <- (nouts.subsets().toSet - nouts))
//      ifta ++= 0 --> 0 by Set(i) ++ ss when FNot(FTrue)
//    mkConn(ifta get i pub nouts.mkString(",") name "-<")
//  }


}
