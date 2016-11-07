package ifta.backend

import ifta.analyse.Simplify
import ifta._

/**
  * Created by jose on 30/09/16.
  */
object Dot {

  /**
    * Convert IFTA into a dot (graphviz) graph
    */
  def apply(iFTA: IFTA): String = {
    val aut = Simplify(iFTA)
//    val edges = toDotEdges(aut.edges.map(Simplify(_)))
    val edges = toDotEdges(aut.edges)
    val invs = aut.locs.map(l => if (aut.cInv.get(l).nonEmpty) s"""{ node [xlabel="${Show(aut.cInv.getOrElse(l,CTrue))}"] $l }""" else "" )
    "digraph G {\n  rankdir=LR;\n  node [margin=0 width=0.3 height=0.2]\n"+
      "  edge [arrowsize=0.7]\n"+
      s"{ rank=min;\n  node [style=filled,shape=doublecircle] ${aut.init} }\n"+
      s"""  label=<<I>fm = ${Show(aut.fm).
                              replaceAll("&","&amp;").
                              replaceAll("-->","&#8594;").
                              replaceAll("<->","&#8596;")}</I>>\n"""+ // feature model
      invs.mkString("\n") + "\n\n" + // invariant labels
      s"$edges}"
  }

  private def toDotEdges(edges: Iterable[Edge]) = {
    val res = new StringBuilder
    for (e <- edges) {
      var temp:List[String] = List()
      if (e.cCons != CTrue)  temp ::= Show(e.cCons)
      if (e.act.nonEmpty)    temp ::= e.act.mkString("/")
      if (e.cReset.nonEmpty) temp ::= e.cReset.map(_ + ":=0").mkString(",")
      if (e.fe != FTrue)     temp ::= Show(e.fe)
      res append
        s"""${e.from} -> ${e.to} [label="${temp.reverse.mkString(",")}"]\n"""
    }
    res.toString()
  }

  /**
    * Convert NIFTA into a dot (graphviz) graph
    */
  def apply(nIFTA: NIFTA): String =
    if (nIFTA.iFTAs.isEmpty) ""
    else nIFTA.iFTAs.map(i => Dot(i)).mkString("\n")

  /**
    * Convert FTA into a dot (graphviz) graph
    */
  def apply(fTA: FTA): String = {
    val aut = Simplify(fTA)
//    val edges = toDotFtaEdges(aut.edges.map(Simplify(_)))
    val edges = toDotFtaEdges(aut.edges)
    val comms: String = aut.committed.mkString(",")
    val invs = aut.locs.map(l => if (aut.cInv.get(l).nonEmpty) s"""{ node [xlabel="${Show(aut.cInv.getOrElse(l,CTrue))}"] $l }""" else "" )
    "digraph G {\n  rankdir=LR;\n  node [margin=0 width=0.3 height=0.2]\n"+
      "  edge [arrowsize=0.7]\n"+
      s"{ rank=min;\n  node [style=filled,shape=doublecircle] ${aut.init} }\n"+ // initial state
      s"{ node [style=filled,shape=square]\n  $comms\n}\n"+ // committed states
      s"""  label=<<I>fm = ${Show(aut.fm).
                              replaceAll("&","&amp;").
                              replaceAll("-->","&#8594;").
                              replaceAll("<->","&#8596;")}</I>>\n"""+ // feature model
      invs.mkString("\n") + "\n\n" + // invariant labels
//      s"""{ node [shape=none]\n  "${Show(aut.fm)}"\n}\n"""+ // feature model
      s"$edges}"
  }

  private def toDotFtaEdges(edges: Iterable[FtaEdge]) = {
    val res = new StringBuilder
    for (e <- edges) {
      var temp:List[String] = List()
      if (e.cCons != CTrue)  temp ::= Show(e.cCons)
                             temp ::= e.act
      if (e.cReset.nonEmpty) temp ::= e.cReset.map(_ + ":=0").mkString(",")
      if (e.fe != FTrue)     temp ::= Show(e.fe)
      res append
        s"""${e.from} -> ${e.to} [label="${temp.reverse.mkString(",")}"]\n"""
    }
    res.toString()
  }

  /**
    * Convert NFTA into a dot (graphviz) graph
    */
  def apply(nFTA:NFTA): String =
    if (nFTA.fTAs.isEmpty) ""
    else nFTA.fTAs.map(i => Dot(i)).mkString("\n")


  def connector(nIFTA: NIFTA): String = {
    s"""digraph G {
      |  rankdir=LR;
      |  node [margin=0.1 width=0.3 height=0.2 shape=rectangle]
      |  edge [arrowsize=0.7]
      |  ${getConnEdges(nIFTA,0,Map())}}
      |}
    """.stripMargin
    }

  def getConnEdges(nIFTA: NIFTA,i:Int,mm:Map[String,(Int,Boolean)]) = {
    var res = ""
    var l = i
    var m = mm
    for (ifta <- nIFTA.iFTAs) {
      if (ifta.shortname!="")
        res += s"""\n  $l [label="${ifta.shortname}"]"""
      else if (ifta.in.nonEmpty || ifta.out.nonEmpty)
          res += s"""\n  $l [label="${(ifta.in++ifta.out).mkString("-")}"]"""

      for (in <- ifta.in) {
        if (m contains in) {
          res += s"""\n  ${m(in)._1} -> $l [label="$in"]"""
          m -= in
        }
        else m += (in->(l,true))
      }
      for (out <- ifta.out) {
        if (m contains out) {
          res += s"""\n  $l -> ${m(out)._1}  [label="$out"]"""
          m -= out
        }
        else m += (out->(l,false))
      }
      l = l+1
    }
    for ((k,(l,b)) <- m) {
      res += s"""\n  { node [shape=ellipse,margin=0]  $k }"""
      if(b) res += s"""\n  "$k" -> $l"""
      else  res += s"""\n  $l -> "$k""""
    }
    res
  }
}
