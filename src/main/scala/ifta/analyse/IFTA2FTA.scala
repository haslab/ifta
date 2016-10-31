package ifta.analyse

import ifta._

/**
  * Created by jose on 11/10/16.
  */
object IFTA2FTA {

  def apply(nIFTA: NIFTA): NFTA = NFTA(nIFTA.iFTAs.map(apply))

  def apply(iFTA: IFTA): FTA = {
    var lastLoc = iFTA.locs.max
    var newLocs = iFTA.locs
    var newEdges: Set[FtaEdge] = Set()
    var committed:Set[Int] = Set()

    for (e <- iFTA.edges)
      genCommitLocs(e, lastLoc, newLocs, newEdges, committed,iFTA.in,iFTA.out) match {
        case (l,n,ed,c) =>
          lastLoc = l
          newLocs = n
          newEdges = ed
          committed = c
      }
//      (lastLoc, newLocs, newEdges, committed) = genCommitLocs(e, lastLoc, newLocs, newEdges, committed)


    FTA(newLocs,iFTA.init,committed,iFTA.act.map(mkAct(_,iFTA.in,iFTA.out)),iFTA.clocks,iFTA.feats,newEdges,iFTA.cInv,iFTA.fm)
  }

  /**
    * Expand an edge with multiple actions into multiple single-action edges with committed locations.
    * Generates the intermediate (committed) locations using a maximum nr of a known location ID.
    * @param e edge to be expanded
    * @param i first fresh location (maximum)
    * @param locs  all known locations
    * @param edges new edges produced so far
    * @param comm new set of committed locations
    * @param ins input actions
    * @param outs output actions
    * @return tuple: fresh location, all locations, new edges, new committed locations
    */
  private def genCommitLocs(e:Edge, i:Int,locs:Set[Int],edges:Set[FtaEdge],comm:Set[Int],ins:Set[String],outs:Set[String]):
      (Int,Set[Int],Set[FtaEdge],Set[Int]) = {
    if (e.act.size == 1)
      (i, locs, edges + FtaEdge(e.from, e.cCons, mkAct(e.act.head,ins,outs), e.cReset, e.fe, e.to), comm)
    else {
      var lloc = i
      var nloc = locs
      var es = edges
      var co = comm
      // new: giving priority to input actions
      val inActs = e.act intersect ins
      val actRound =
        if (inActs.nonEmpty) inActs // just input actions
        else                 e.act  // just non-input actions
      // (end new part)
      for (a <- actRound) {
        lloc += 1
        nloc += lloc
        co += lloc
        es += FtaEdge(e.from, e.cCons, mkAct(a,ins,outs), e.cReset, e.fe, lloc)
        genCommitLocs(Edge(lloc,e.cCons,e.act - a,e.cReset,e.fe,e.to), lloc, nloc, es, co,ins,outs) match {
          case (x,y,w,z) => {lloc = x; nloc = y; es = w; co = z}
        }
//        (lloc, nloc, es, co) = genCommitLocs(e by (e.act - a), lloc, nloc, es, co)
      }
      (lloc, nloc, es, co)
    }
  }

  private def mkAct(a:String,ins:Set[String],outs:Set[String]) =
    if (ins contains a) a+"?" else if (outs contains a) a+"!" else a
}
