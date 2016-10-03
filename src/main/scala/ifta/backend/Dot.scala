package ifta.backend

import ifta.{Edge, IFTA}

/**
  * Created by jose on 30/09/16.
  */
object Dot {
  def apply(ifta: IFTA): String = {
    val edges = toDotEdges(ifta.edges)
    "digraph G {\n  rankdir=LR;\n  node [margin=0 width=0.2 height=0.2 label=\"\"]\n"++
      "  edge [arrowsize=0.7]\n"++
      s"$edges}"
  }

  def toDotEdges(edges: Iterable[Edge]) = {
    val res = new StringBuilder
    for (e <- edges)
      res append s"""${e.from} -> ${e.to} [label="${e.act.mkString("/")}"]\n"""
    res.toString()
  }

}
