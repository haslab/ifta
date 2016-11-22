package ifta.reo

import ifta.DSL._
import ifta.analyse.Simplify
import ifta._

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
object FamConnectors {

  /** Wrap for IFTAs extended with `relax` and `exclusive` methods. */
  class Conn(locs:Set[Int], init:Int, act:Set[String], clocks:Set[String]
             , feats:Set[String], edges:Set[Edge], cInv:Map[Int,ClockCons], fm:FExp
             , in:Set[String], out:Set[String], aps:Map[Int,String], shortname:String="")
  extends IFTA(locs,init,act,clocks,feats,edges,cInv,fm,in,out,aps,shortname) {
    def relax: Conn =
      new Conn(locs,init,act,clocks,act.map(v),edges.map(e => relaxEdge(e,act)),cInv,fm,in,out,aps,shortname)
    private def relaxEdge(e: Edge,acts:Set[String]):Edge = {
//      val neg = if (e.fe == FNot(FTrue)) (acts -- e.act).map(v(_)) else Set()
//      val fe = e.act.foldRight[FExp](FTrue)((s, fs) => v(s) && fs) && mkFAnd(neg.map(not(_)))
      val fe = e.act.foldRight[FExp](FTrue)((s, fs) => v(s) && fs)
      Edge(e.from, e.cCons, e.act, e.cReset, fe , e.to)
    }
    override def when(f: FExp): Conn =
      new Conn(locs,init,act,clocks,feats++f.feats,edges,cInv,fm && f,in,out,aps,shortname)

    def exclusive(ps:String):Conn =
      new Conn(locs,init,act,clocks,act,
        edges.map(exclusiveEdge(Set(),ps.split(",").toSet)),
        cInv,fm,in,out,aps,shortname)
    def exclusive(pair:(String,String)): Conn =
      new Conn(locs,init,act,clocks,act,
        edges.map(exclusiveEdge(pair._1.split(",").toSet,pair._2.split(",").toSet)),
        cInv,fm,in,out,aps,shortname)
    def exclusiveEdge(as:Set[String],ps:Set[String])(e:Edge): Edge =
      if ((as == e.act) || as.isEmpty)
        Edge(e.from,e.cCons,e.act,e.cReset,
        e.act.foldRight[FExp](FTrue)((s,fs)=>v(s)&&fs) &&
          //              (as -- e.act).foldRight[FExp](FTrue)((s,fs)=>not(v(s))&&fs),
          (ps -- e.act).foldRight[FExp](FTrue)((s,fs)=>not(v(s))&&fs),
        e.to)
      else e

//    def exclude(p:String): Conn = exclude(Set(p))
//    def exclude(ps:Set[String]): Conn =
//      new Conn(locs,init,act,clocks,act,edges.map(excludeEdge(ps,act)),cInv,fm,in,out,aps,shortname)
//    private def excludeEdge(ps:Set[String],as:Set[String])(e:Edge): Edge =
//      if (ps != e.act) e
//      else Edge(e.from,e.cCons,e.act,e.cReset,
//        e.act.foldRight[FExp](FTrue)((s,fs)=>v(s)&&fs) &&
//          (as -- e.act).foldRight[FExp](FTrue)((s,fs)=>not(v(s))&&fs),
//        e.to)
  }


  // auxilary constructions
  private def v(s:String) = "v_"+s
  private def mkFeat(act:Set[String]):Set[FExp] = act.map(a => Feat(v(a)))
  private def mkFAnd(feats:Set[FExp]):FExp = Simplify(feats.fold(FTrue)(_&&_))
  private def mkFOr(feats:Set[FExp]):FExp = Simplify(feats.fold(FNot(FTrue))(_||_))
  private def mkConn(i:IFTA) =
    new Conn(i.locs,i.init,i.act,i.clocks,i.feats,i.edges,i.cInv,i.fm,i.in,i.out,i.aps,i.shortname)


  def fifo(i: String, o: String) = {
    val feat = v(s"${i}_${o}")
    mkConn(newifta ++ (
      0 --> 1 by s"$i" when feat,
      1 --> 0 by s"$o" when feat,
      0 --> 0 by s"$i" when FNot(FTrue)
      ) get i pub o name "[]")
  }

  def fifofull(i: String, o: String) = {
    val feat = v(s"${i}_${o}")
    mkConn(newifta ++ (
      0 --> 1 by s"$o" when feat,
      1 --> 0 by s"$i" when feat
      ) get i pub o name "[.]")
  }

  def sync(i: String, o: String) = {
    val feat = v(s"${i}_${o}")
    mkConn(newifta ++ (
      0 --> 0 by s"$i,$o" when feat,
      0 --> 0 by s"$i" when FNot(FTrue)
      ) get i pub o name "->")
  }

  def sdrain(i: String, i2: String) = {
    val feat = v(s"${i}_${i2}")
    mkConn(newifta ++ (
      0 --> 0 by s"$i,$i2" when feat,
      0 --> 0 by s"$i" when FNot(FTrue),
      0 --> 0 by s"$i2" when FNot(FTrue)
      ) get i get i2 name ">-<")
  }

  def asdrain(i: String, i2: String) = {
    val feat = v(s"${i}_${i2}")
    mkConn(newifta ++ (
      0 --> 0 by s"$i" when feat,
      0 --> 0 by s"$i2" when feat
      ) get i get i2 name ">|<")
  }

  def router(i: String, o1: String, o2: String, outs: String*) = {
    val feat = v(s"${i}_${o1}_$o2" + (if (outs.isEmpty) "" else outs.mkString("_", "_", "")))
    val nouts = Set(o1, o2) ++ outs.toSet
    var ifta = newifta ++ (0 --> 0 by i when FNot(FTrue))
    for (o <- nouts)
      ifta ++= (0 --> 0 by Set(i, o) when feat)
    mkConn(ifta get i pub nouts.mkString(",") name "Xor")
  }

  def join(i: String, i2: String, inOrO: String, rest: String*) = {
    val feat = v(s"${i}_${i2}_$inOrO" + (if (rest.isEmpty) "" else rest.mkString("_", "_", "")))
    val o = if (rest.isEmpty) inOrO else rest.last
    val ins = if (rest.nonEmpty) Set(i, i2, inOrO) ++ (rest.toSet - o) else Set(i, i2)
    var ifta = newifta ++ (0 --> 0 by Set(o)++ins when feat)
    val ss = (ins + o).subsets().toSet - Set() - Set(o)
    for (s <- ss - (Set(o)++ins))
      ifta ++= (0 --> 0 by s when FNot(FTrue))
    mkConn(ifta get ins.mkString(",") pub o name "+")
  }

  def merger(i: String, i2: String, inOrO: String, rest: String*) = {
    val feat = v(s"${i}_${i2}_$inOrO" + (if (rest.isEmpty) "" else rest.mkString("_", "_", "")))
    val o = if (rest.isEmpty) inOrO else rest.last
    var ifta = newifta
    val ins = if (rest.nonEmpty) Set(i, i2, inOrO) ++ (rest.toSet - o) else Set(i, i2)
    for (i <- ins)
      ifta ++= (
        0 --> 0 by Set(i, o) when feat,
        0 --> 0 by Set(i) when FNot(FTrue)
        )
    mkConn(ifta get ins.mkString(",") pub o name ">-") //when v(o) --> mkFOr(mkFeat(ins)) name ">-")
  }

  def repl(i: String, o1: String, o2: String, outs: String*) = {
    val feat = v(s"${i}_${o1}_$o2" + (if (outs.isEmpty) "" else outs.mkString("_", "_", "")))
    val nouts = Set(o1, o2) ++ outs.toSet
    var ifta = newifta ++ (0 --> 0 by Set(i)++nouts when feat)
    for (ss <- (nouts.subsets().toSet - nouts))
      ifta ++= 0 --> 0 by Set(i) ++ ss when FNot(FTrue)
    mkConn(ifta get i pub nouts.mkString(",") name "-<")
  }
}
