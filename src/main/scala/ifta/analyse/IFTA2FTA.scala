package ifta.analyse

import ifta.{Edge, FTA, FtaEdge, IFTA}

/**
  * Created by jose on 11/10/16.
  */
object IFTA2FTA {
  def apply(iFTA: IFTA): FTA = {
    var lastLoc = iFTA.locs.max
    var newLocs = iFTA.locs
    var newEdges: Set[FtaEdge] = Set()
    var committed:Set[Int] = Set()

    for (e <- iFTA.edges)
      genCommitLocs(e, lastLoc, newLocs, newEdges, committed) match {
        case (l,n,ed,c) =>
          lastLoc = l
          newLocs = n
          newEdges = ed
          committed = c
      }
//      (lastLoc, newLocs, newEdges, committed) = genCommitLocs(e, lastLoc, newLocs, newEdges, committed)


    FTA(newLocs,iFTA.init,committed,iFTA.act,iFTA.clocks,iFTA.feats,newEdges,iFTA.cInv,iFTA.fm)
  }

  private def genCommitLocs(e:Edge, i:Int,locs:Set[Int],edges:Set[FtaEdge],comm:Set[Int]):
      (Int,Set[Int],Set[FtaEdge],Set[Int]) = {
    if (e.act.size == 1) (i, locs, edges + FtaEdge(e.from, e.cCons, e.act.head, e.cReset, e.fe, e.to), comm)
    else {
      var lloc = i
      var nloc = locs
      var es = edges
      var co = comm
      for (a <- e.act) {
        lloc += 1
        nloc += lloc
        co += lloc
        es += FtaEdge(e.from, e.cCons, a, e.cReset, e.fe, lloc)
        genCommitLocs(Edge(lloc,e.cCons,e.act - a,e.cReset,e.fe,e.to), lloc, nloc, es, co) match {
          case (x,y,w,z) => {lloc = x; nloc = y; es = w; co = z}
        }
//        (lloc, nloc, es, co) = genCommitLocs(e by (e.act - a), lloc, nloc, es, co)
      }
      (lloc, nloc, es, co)
    }
  }
}
