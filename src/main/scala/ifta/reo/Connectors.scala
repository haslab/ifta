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
object Connectors {

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

    def excludes(ps:String):Conn =
      new Conn(locs,init,act,clocks,feats,
        edges.map(excludesEdge(Set(),ps.split(",").toSet)),
        cInv,fm,in,out,aps,shortname)
    def excludes(pair:(String,String)): Conn =
      new Conn(locs,init,act,clocks,feats,
        edges.map(excludesEdge(pair._1.split(",").toSet,pair._2.split(",").toSet)),
        cInv,fm,in,out,aps,shortname)
    def excludesEdge(as:Set[String], ps:Set[String])(e:Edge): Edge =
      if ((as == e.act) || as.isEmpty)
        Edge(e.from,e.cCons,e.act,e.cReset,
        e.act.foldRight[FExp](FTrue)((s,fs)=>v(s)&&fs) &&
          //              (as -- e.act).foldRight[FExp](FTrue)((s,fs)=>not(v(s))&&fs),
          (ps -- e.act).foldRight[FExp](FTrue)((s,fs)=>not(v(s))&&fs),
        e.to)
      else e

    def requires(pair:(String,String)):Conn =
      new Conn(locs,init,act,clocks,feats,
        edges.map(requiresEdge(pair._1.split(",").toSet,pair._2.split(",").toSet)),
        cInv,fm,in,out,aps,shortname)
    def requiresEdge(as:Set[String],ps:Set[String])(e:Edge): Edge =
      if ((as == e.act) || as.isEmpty)
        Edge(e.from,e.cCons,e.act,e.cReset,
          e.act.foldRight[FExp](FTrue)((s,fs)=>v(s)&&fs) &&
            //              (as -- e.act).foldRight[FExp](FTrue)((s,fs)=>not(v(s))&&fs),
            (ps -- e.act).foldRight[FExp](FTrue)((s,fs)=>v(s)&&fs),
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


//  def fifo(i: String, o: String) = {
//    val feat = v(s"${i}_${o}")
//    mkConn(newifta ++ (
//      0 --> 1 by s"$i" when feat,
//      1 --> 0 by s"$o" when feat
////      0 --> 0 by s"$i" when FNot(FTrue)
//      ) get i pub o name "[]")
//  }
  def fifo(i: String, o: String) = {
//    val feat = v(s"${i}_${o}")
    mkConn(newifta ++ (
      0 --> 1 by s"$i" when v(i) && v(o),
      1 --> 0 by s"$o" when v(i) && v(o)
      //      0 --> 0 by s"$i" when FNot(FTrue)
    ) get i pub o name "[]" when v(i) <-> v(o))
  }

//  def fifofull(i: String, o: String) = {
//    val feat = v(s"${i}_${o}")
//    mkConn(newifta ++ (
//      0 --> 1 by s"$o" when feat,
//      1 --> 0 by s"$i" when feat
//      ) get i pub o name "[.]")
//  }

  def fifofull(i: String, o: String) = {
//    val feat = v(s"${i}_${o}")
    mkConn(newifta ++ (
      0 --> 1 by s"$o" when v(i) && v(o),
      1 --> 0 by s"$i" when v(i) && v(o)
    ) get i pub o name "[.]" when v(i) <-> v(o))
  }

//  def sync(i: String, o: String) = {
//    val feat = v(s"${i}_${o}")
//    mkConn(newifta ++ (
//      0 --> 0 by s"$i,$o" when feat,
//      0 --> 0 by s"$i" when FNot(FTrue)
//      ) get i pub o name "->")
//  }

  def sync(i: String, o: String) = {
//    val feat = v(s"${i}_${o}")
    mkConn(newifta ++ (
      0 --> 0 by s"$i,$o" when v(i) && v(o)
//      0 --> 0 by s"$i" when FNot(FTrue)
    ) get i pub o name "->" when v(i) <-> v(o))
  }

//  def lossy(i:String,o:String) = {
//    val feat = v(s"${i}_${o}")
//    mkConn(newifta ++ (
//      0 --> 0 by s"$i,$o" when feat,
//      0 --> 0 by s"$i" when feat
//    )get i pub o name "-->")
//  }

  def lossy(i:String,o:String) = {
//    val feat = v(s"${i}_${o}")
    mkConn(newifta ++ (
      0 --> 0 by s"$i,$o" when v(i) && v(o),
      0 --> 0 by s"$i" when v(i)
    )get i pub o name "-->" when v(i) <-> v(o))
  }

//  def sdrain(i: String, i2: String) = {
//    val feat = v(s"${i}_${i2}")
//    mkConn(newifta ++ (
//      0 --> 0 by s"$i,$i2" when feat,
//      0 --> 0 by s"$i" when FNot(FTrue),
//      0 --> 0 by s"$i2" when FNot(FTrue)
//      ) get i get i2 name ">-<")
//  }

  def sdrain(i: String, i2: String) = {
//    val feat = v(s"${i}_${i2}")
    mkConn(newifta ++ (
      0 --> 0 by s"$i,$i2" when v(i) && v(i2),
//      0 --> 0 by s"$i" when FNot(FTrue),
//      0 --> 0 by s"$i2" when FNot(FTrue)
    ) get i get i2 name ">-<" when v(i) <-> v(i2))
  }

//  def asdrain(i: String, i2: String) = {
//    val feat = v(s"${i}_${i2}")
//    mkConn(newifta ++ (
//      0 --> 0 by s"$i" when feat,
//      0 --> 0 by s"$i2" when feat
//      ) get i get i2 name ">|<")
//  }

  def asdrain(i: String, i2: String) = {
//    val feat = v(s"${i}_${i2}")
    mkConn(newifta ++ (
      0 --> 0 by s"$i" when v(i) && v(i2),
      0 --> 0 by s"$i2" when v(i) && v(i2)
    ) get i get i2 name ">|<" when v(i) <-> v(i2))
  }

//  def router(i: String, o1: String, o2: String, outs: String*) = {
//    val feat = v(s"${i}_${o1}_$o2" + (if (outs.isEmpty) "" else outs.mkString("_", "_", "")))
//    val nouts = Set(o1, o2) ++ outs.toSet
//    var ifta = newifta ++ (0 --> 0 by i when FNot(FTrue))
//    for (o <- nouts)
//      ifta ++= (0 --> 0 by Set(i, o) when feat)
//    mkConn(ifta get i pub nouts.mkString(",") name "Xor")
//  }

  def router(i: String, o1: String, o2: String, outs: String*) = {
//    val feat = v(s"${i}_${o1}_$o2" + (if (outs.isEmpty) "" else outs.mkString("_", "_", "")))
    val nouts = Set(o1, o2) ++ outs.toSet
    var ifta = newifta //++ (0 --> 0 by i when FNot(FTrue))
    for (o <- nouts)
      ifta ++= (0 --> 0 by Set(i, o) when v(i) && v(o))
    val fm =  mkFAnd(mkFeat(nouts++Set(i))) || FNot(mkFOr(mkFeat(nouts++Set(i))))// Feat(v(i)) <-> (mkFAnd(mkFeat(nouts)))
    mkConn(ifta get i pub nouts.mkString(",") name "Xor" when fm )
  }

  def vrouter(i:String,o1:String,o2:String, outs:String*) = {
    val nouts = Set(o1, o2) ++ outs.toSet
    var ifta = newifta //++ (0 --> 0 by i when FNot(FTrue))
    for (o <- nouts)
      ifta ++= (0 --> 0 by Set(i, o) when v(i) && v(o))
    val fm =  Feat(v(i)) <-> (mkFOr(mkFeat(nouts)))
    mkConn(ifta get i pub nouts.mkString(",") name "Xor" when fm )

  }

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

  def join(i: String, i2: String, inOrO: String, rest: String*) = {
//    val feat = v(s"${i}_${i2}_$inOrO" + (if (rest.isEmpty) "" else rest.mkString("_", "_", "")))
    val o = if (rest.isEmpty) inOrO else rest.last
    val ins = if (rest.nonEmpty) Set(i, i2, inOrO) ++ (rest.toSet - o) else Set(i, i2)
    var ifta = newifta ++ (0 --> 0 by Set(o)++ins when mkFAnd(mkFeat(Set(o)++ins)))
//    val ss = (ins + o).subsets().toSet - Set() - Set(o)
//    for (s <- ss - (Set(o)++ins))
//      ifta ++= (0 --> 0 by s when FNot(FTrue))
    var fm = mkFAnd(mkFeat(ins++Set(o))) || FNot(mkFOr(mkFeat(ins++Set(o))))//Feat(v(o)) <-> (mkFAnd(mkFeat(ins)))
    mkConn(ifta get ins.mkString(",") pub o name "+" when fm)
  }


  def vjoin(i: String, i2: String, inOrO: String, rest: String*) = {
    val o = if (rest.isEmpty) inOrO else rest.last
    val ins = if (rest.nonEmpty) Set(i, i2, inOrO) ++ (rest.toSet - o) else Set(i, i2)
    var ifta = newifta //++ (0 --> 0 by Set(o)++ins when mkFAnd(mkFeat(Set(o)++ins)))
    val ss = (ins + o).subsets().toSet - Set() - Set(o)
        for (s <- ss - (Set(o)++ins))
          ifta ++= (0 --> 0 by s when mkFAnd(mkFeat(s)) && not(mkFOr(mkFeat((ins+o)--s))))
    var fm = Feat(v(o)) <-> (mkFOr(mkFeat(ins)))
    mkConn(ifta get ins.mkString(",") pub o name "+" when fm)
  }

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
//    mkConn(ifta get ins.mkString(",") pub o name ">-") //when v(o) --> mkFOr(mkFeat(ins)) name ">-")
//  }

  def merger(i: String, i2: String, inOrO: String, rest: String*) = {
//    val feat = v(s"${i}_${i2}_$inOrO" + (if (rest.isEmpty) "" else rest.mkString("_", "_", "")))
    val o = if (rest.isEmpty) inOrO else rest.last
    var ifta = newifta
    val ins = if (rest.nonEmpty) Set(i, i2, inOrO) ++ (rest.toSet - o) else Set(i, i2)
    for (i <- ins)
      ifta ++= (
        0 --> 0 by Set(i, o) when v(i) && v(o) //,
//        0 --> 0 by Set(i) when FNot(FTrue)
      )
    var fm = mkFAnd(mkFeat(ins++Set(o))) || FNot(mkFOr(mkFeat(ins++Set(o))))//v(o) <-> (mkFAnd(mkFeat(ins)))
    mkConn(ifta get ins.mkString(",") pub o name ">-" when fm) //when v(o) --> mkFOr(mkFeat(ins)) name ">-")
  }

  def vmerger(i: String, i2: String, inOrO: String, rest: String*) = {
    val o = if (rest.isEmpty) inOrO else rest.last
    var ifta = newifta
    val ins = if (rest.nonEmpty) Set(i, i2, inOrO) ++ (rest.toSet - o) else Set(i, i2)
    for (i <- ins)
      ifta ++= (
        0 --> 0 by Set(i, o) when v(i) && v(o)
        )
    var fm = Feat(v(o)) <-> (mkFOr(mkFeat(ins)))
    mkConn(ifta get ins.mkString(",") pub o name ">-" when fm) //when v(o) --> mkFOr(mkFeat(ins)) name ">-")
  }

//  def repl(i: String, o1: String, o2: String, outs: String*) = {
//    val feat = v(s"${i}_${o1}_$o2" + (if (outs.isEmpty) "" else outs.mkString("_", "_", "")))
//    val nouts = Set(o1, o2) ++ outs.toSet
//    var ifta = newifta ++ (0 --> 0 by Set(i)++nouts when feat)
//    for (ss <- (nouts.subsets().toSet - nouts))
//      ifta ++= 0 --> 0 by Set(i) ++ ss when FNot(FTrue)
//    mkConn(ifta get i pub nouts.mkString(",") name "-<")
//  }

  def repl(i: String, o1: String, o2: String, outs: String*) = {
//    val feat = v(s"${i}_${o1}_$o2" + (if (outs.isEmpty) "" else outs.mkString("_", "_", "")))
    val nouts = Set(o1, o2) ++ outs.toSet
    var ifta = newifta ++ (0 --> 0 by Set(i)++nouts when mkFAnd(mkFeat(Set(i)++nouts)))
//    for (ss <- (nouts.subsets().toSet - nouts))
//      ifta ++= 0 --> 0 by Set(i) ++ ss when FNot(FTrue)
    var fm = mkFAnd(mkFeat(nouts++Set(i))) || FNot(mkFOr(mkFeat(nouts++Set(i))))//v(i) <-> (mkFAnd(mkFeat(nouts)))
    mkConn(ifta get i pub nouts.mkString(",") name "-<" when fm)
  }

  def vrepl(i: String, o1: String, o2: String, outs: String*) = {
    val nouts = Set(o1, o2) ++ outs.toSet
    var ifta = newifta // ++ (0 --> 0 by Set(i)++nouts when mkFAnd(mkFeat(Set(i)++nouts)))
    val ss = nouts.subsets().toSet - Set()
    for (s <- ss)
      ifta ++= 0 --> 0 by Set(i) ++ s when v(i) && mkFAnd(mkFeat(s)) && not(mkFOr(mkFeat(nouts--s)))
    var fm = Feat(v(i)) <-> (mkFOr(mkFeat(nouts)))
    mkConn(ifta get i pub nouts.mkString(",") name "-<" when fm)
  }

  def writer(o:String) = {
    val feat = v(o)
    mkConn(newifta ++ (
      0 --> 0 by o when feat
    ) pub o name "writer")
  }

  def reader(i:String) = {
    val feat = v(i)
    mkConn(newifta ++ (
      0 --> 0 by i when feat
    ) get i name "reader")
  }

  def timer(i:String,o:String,t:Int)=
    mkConn(
      newifta ++ (
        0 --> 1 by i when v(i) && v(o) reset "c",
        1 --> 0 by o when v(i) && v(o) cc ("c" equal t)
      ) get i pub o inv(1, "c"<=t) when v(i) <-> v(o) name s"($t)"
    )

  def noSink(o:String, outs: String*) = {
    val nouts = Set(o) ++ outs.toSet
    mkConn(newifta pub nouts.mkString(",") name "noSnk")
  }

  def noSrc(i:String, inputs:String*) = {
    val nints = Set(i) ++ inputs.toSet
    mkConn(newifta get nints.mkString(",") name "noSrc")
  }

  def sink(i:String, inputs:String*) = {
    val nints = Set(i) ++ inputs.toSet
    var ifta = newifta
    nints.foreach(in => ifta ++= 0 --> 0 by in when v(in))
    val fm = mkFAnd(mkFeat(nints)) || not(mkFOr(mkFeat(nints)))
    mkConn(ifta get nints.mkString(",") when fm)
  }

  def source(o:String, outs:String*) = {
    val nouts = Set(o) ++ outs.toSet
    var ifta = newifta ++ (
        0 --> 0 by nouts.mkString(",") when mkFAnd(mkFeat(nouts))
      )
    val fm = mkFAnd(mkFeat(nouts)) || not(mkFOr(mkFeat(nouts)))
    mkConn(ifta pub nouts.mkString(",") when fm)
  }

  /** Typical mixed node */
  def mixed(ins:Set[String],outs:Set[String]) = {
    var ifta = newifta
    ins.foreach(i =>
      ifta ++= 0 --> 0 by (Set(i)++outs).mkString(",") when v(i) && mkFAnd(mkFeat(outs))
    )
    val fm = mkFAnd(mkFeat(ins++outs)) || not(mkFOr(mkFeat(ins++outs)))
    mkConn( ifta get ins.mkString(",") pub outs.mkString(",") when fm)
  }

  /** Variable mixed node (typical mixed node with variability) */
  def vmixed(ins:Set[String],outs:Set[String]) = {
    var ifta = newifta
    val ss =  outs.subsets().toSet - Set() // all subsets of outs - emptyset
    ins.foreach( i =>
      ss.foreach( os =>
        ifta ++= 0 --> 0 by (Set(i)++os) when
          v(i) && mkFAnd(mkFeat(os)) && not(mkFOr(mkFeat(outs--os))))
    )
    val fm = mkFOr(mkFeat(ins)) <-> mkFOr(mkFeat(outs))
    mkConn( ifta get ins.mkString(",") pub outs.mkString(",") when fm)
  }

  /** Variable mixed node where the inputs are variable but not the outputs */
  def vmrg2dupl(ins:Set[String],outs:Set[String]) = {
    var ifta = newifta
    ins.foreach( i =>
        ifta ++= 0 --> 0 by (Set(i)++outs) when v(i) && mkFAnd(mkFeat(outs))
    )
    val fm = mkFAnd(mkFeat(outs)) && mkFOr(mkFeat(ins))
    mkConn( ifta get ins.mkString(",") pub outs.mkString(",") when fm)
  }

  /** Variable mixed node where the inputs fixed but the outputs are variables */
  def mrg2vdupl(ins:Set[String],outs:Set[String]) = {
    var ifta = newifta
    val ss =  outs.subsets().toSet - Set() // all subsets of outs - emptyset
    ins.foreach( i =>
      ss.foreach( os =>
        ifta ++= 0 --> 0 by (Set(i)++os) when
          v(i) && mkFAnd(mkFeat(os)) && not(mkFOr(mkFeat(outs--os))))
    )
    val fm = mkFAnd(mkFeat(ins)) && mkFOr(mkFeat(outs))
    mkConn( ifta get ins.mkString(",") pub outs.mkString(",") when fm)
  }

  /** Mixed node that behaves as a xor */
  def mixedxor(ins:Set[String],outs:Set[String]) = {
    var ifta = newifta
    ins.foreach( i =>
      outs.foreach( o =>
        ifta ++= 0 --> 0 by (Set(i)+o) when
          v(i) && v(o))
    )
    val fm = mkFAnd(mkFeat(ins++outs)) || not(mkFOr(mkFeat(ins++outs)))
    mkConn( ifta get ins.mkString(",") pub outs.mkString(",") when fm)
  }

  /** Variable mixed node that behaves as a xor */
  def vmixedxor(ins:Set[String],outs:Set[String]) = {
    var ifta = newifta
    ins.foreach( i =>
      outs.foreach( o =>
        ifta ++= 0 --> 0 by (Set(i)+o) when
          v(i) && v(o))
    )
    val fm = mkFOr(mkFeat(ins)) <-> mkFOr(mkFeat(outs))
    mkConn( ifta get ins.mkString(",") pub outs.mkString(",") when fm)
  }

}
