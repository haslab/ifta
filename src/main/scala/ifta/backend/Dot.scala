package ifta.backend

import ifta.analyse.Simplify
import ifta.{Edge, IFTA}

/**
  * Created by jose on 30/09/16.
  */
object Dot {
  def apply(ifta: IFTA): String = {
    val edges = toDotEdges(ifta.edges.map(Simplify(_)))
    "digraph G {\n  rankdir=LR;\n  node [margin=0 width=0.3 height=0.2]\n"++
      "  edge [arrowsize=0.7]\n"++
      s"$edges}"
  }

  def toDotEdges(edges: Iterable[Edge]) = {
    val res = new StringBuilder
    for (e <- edges)
      res append s"""${e.from} -> ${e.to} [label="${Show(e.cCons)},${e.act.mkString("/")},"""+
                 s"""${e.cReset.map(_+":=0").mkString(",")},${Show(e.fe)}"]\n"""
    res.toString()
  }

}
