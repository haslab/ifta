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
  def apply(aut: IFTA): String = {
    val edges = toDotEdges(aut.edges.map(Simplify(_)))
    "digraph G {\n  rankdir=LR;\n  node [margin=0 width=0.3 height=0.2]\n"+
      "  edge [arrowsize=0.7]\n"+
      s"{ rank=min;\n  node [style=filled,shape=doublecircle] ${aut.init} }\n"+
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
  def toDot(nIFTA: NIFTA) =
    if (nIFTA.iFTAs.isEmpty) ""
    else nIFTA.iFTAs.map(i => Dot(i)).mkString("\n")


  /**
    * Convert FTA into a dot (graphviz) graph
    */
  def apply(aut: FTA): String = {
    val edges = toDotFtaEdges(aut.edges.map(Simplify(_)))
    val comms: String = aut.committed.mkString(",")
    "digraph G {\n  rankdir=LR;\n  node [margin=0 width=0.3 height=0.2]\n"+
      "  edge [arrowsize=0.7]\n"+
      s"{ rank=min;\n  node [style=filled,shape=doublecircle] ${aut.init} }\n"+ // initial state
      s"{ node [style=filled,shape=square]\n  $comms\n}\n"+ // committed states
      s"$edges}"
  }

  private def toDotFtaEdges(edges: Iterable[FtaEdge]) = {
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
}
