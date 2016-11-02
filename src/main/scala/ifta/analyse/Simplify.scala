package ifta.analyse

import ifta._

/**
  * Created by jose on 04/10/16.
  */
object Simplify {

  def apply(iFTA:IFTA): IFTA = {
    val i = remUnreach(iFTA)
    IFTA(i.locs, i.init, i.act, i.clocks, i.feats, i.edges.map(apply), i.cInv.mapValues(apply), apply(i.fm), i.in, i.out)
  }

  def apply(i:FTA): FTA =
    FTA(i.locs,i.init,i.committed,i.act,i.clocks,i.feats,i.edges.map(apply),i.cInv.mapValues(apply),apply(i.fm))

  def apply(f:FExp): FExp = f match {
    case FTrue => FTrue
    case Feat(_)     => f
    case FAnd(e1,e2) => (apply(e1),apply(e2)) match {
      case (FTrue, e) => e
      case (e,FTrue) => e
      case (FNot(FTrue), _) => FNot(FTrue)
      case (_,FNot(FTrue)) => FNot(FTrue)
      case (e3, e4) => FAnd(e3,e4)
    }
    case FOr(e1,e2) => (apply(e1),apply(e2)) match {
      case (FTrue, e) => FTrue
      case (e, FTrue) => FTrue
      case (FNot(FTrue), e) => e
      case (e, FNot(FTrue)) => e
      case (e3, e4) => FOr(e3, e4)
    }
    case FNot(FNot(e)) => apply(e)
    case FNot(e) => FNot(apply(e))
  }

  def apply(cc:ClockCons): ClockCons = cc match {
    case CTrue | LT(_,_) | GT(_,_) | LE(_,_) | GE(_,_) => cc
    case CAnd(CTrue, cc2) => apply(cc2)
    case CAnd(cc1, CTrue) => apply(cc1)
    case CAnd(cc1, cc2) => CAnd(apply(cc1),apply(cc2))
  }

  def apply(e:Edge):Edge =
    Edge(e.from,apply(e.cCons),e.act,e.cReset,apply(e.fe),e.to)

  def apply(e:FtaEdge):FtaEdge =
    FtaEdge(e.from,apply(e.cCons),e.act,e.cReset,apply(e.fe),e.to)

  /**
    * Removes unreachable locations and edges
    * @param ifta
    * @return new IFTA without unreachable locations and edges
    */
  def remUnreach(ifta: IFTA):IFTA = {
    var visited:Set[Int] = Set(ifta.init)
    var newedges:Set[Edge] = Set()
    for (e <- ifta.edges.filter(_.from == ifta.init)) {
      newedges += e
      if (!(visited contains e.to)) {
        visit(ifta,e.to,visited,newedges) match {
          case (v,ne) => {visited = v + e.to; newedges = ne}
        }
      }
    }
    IFTA(visited,ifta.init,ifta.act,ifta.clocks,ifta.feats,newedges,ifta.cInv,ifta.fm,ifta.in, ifta.out)
  }

  /**
    * Visits all reachable locations from a given location and a IFTA
    * updating the set of visited locations and reachable edges
    * @param ifta
    * @param loc location to visit
    * @param v visited locations
    * @param ne reachable edges
    * @return tuple: new visited locations, new reachable edges
    */
  private def visit(ifta: IFTA,loc:Int,v:Set[Int],ne:Set[Edge]):
  (Set[Int],Set[Edge]) = {
    var visited = v + loc
    var newedges = ne
    for (e <- ifta.edges.filter(_.from == loc)) {
      newedges += e
      if (!(visited contains e.to))
        visit(ifta,e.to,visited,newedges) match {
          case (ved,nes) => {visited = ved; newedges = nes}
        }
    }
    (visited, newedges)
  }

}
