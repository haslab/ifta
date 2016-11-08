package ifta.analyse

import ifta._

/**
  * Created by jose on 04/10/16.
  */
object Simplify {

  def apply(iFTA:IFTA): IFTA = {
    val i = removeUnreach(iFTA)
    IFTA(i.locs, i.init, i.act, i.clocks, i.feats, i.edges.map(apply), i.cInv.mapValues(apply), apply(i.fm), i.in, i.out,i.aps,i.shortname)
  }

  def apply(i:FTA): FTA =
    FTA(i.locs,i.init,i.committed,i.act,i.clocks,i.feats,i.edges.map(apply),i.cInv.mapValues(apply),apply(i.fm),i.aps,i.shortname)

  def apply(f:FExp): FExp = f match {
    case FTrue => FTrue
    case Feat(_)     => f
    case FAnd(e1,e2) => (apply(e1),apply(e2)) match {
      case (FTrue, e) => e
      case (e,FTrue) => e
      case (FNot(FTrue), _) => FNot(FTrue)
      case (_,FNot(FTrue)) => FNot(FTrue)
      case (FNot(e3),e4) => if (e3 == e4) FNot(FTrue) else FAnd(FNot(e3),e4)
      case (e3,FNot(e4)) => if (e3 == e4) FNot(FTrue) else FAnd(e3,FNot(e4))
      case (e3, e4) => if (e3 == e4) e3 else FAnd(e3,e4)
    }
    case FOr(e1,e2) => (apply(e1),apply(e2)) match {
      case (FTrue, eSolver) => FTrue
      case (e, FTrue) => FTrue
      case (FNot(FTrue), e) => e
      case (e, FNot(FTrue)) => e
      case (FNot(e3),e4) => if (e3 == e4) FTrue else FOr(FNot(e3),e4)
      case (e3,FNot(e4)) => if (e3 == e4) FTrue else FOr(e3,FNot(e4))
      case (e3, e4) => if (e3 == e4) e3 else FOr(e3, e4)
    }
    case FNot(FNot(e)) => apply(e)
    case FNot(e) => FNot(apply(e))
    case FImp(e1,e2) => (apply(e1), apply(e2)) match {
      case (FNot(FTrue),_) => FTrue//FNot(FTrue)
      case (_,FTrue) => FTrue
      case (e3,e4) => FImp(e3,e4)
    }
    case FEq(e1,e2) => (apply(e1), apply(e2)) match {
      case (FTrue,e3) => e3
      case (e3,FTrue) => e3
      case (FNot(FTrue),e3) => FNot(e3)
      case (e3,FNot(FTrue)) => FNot(e3)
      case (e3,e4) => if (e3==e4) FTrue else FEq(e3,e4)
    }
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
    * Remove syntactic sugar (<->,-->)
    * @param f
    * @return
    */
  def removeSS(f:FExp):FExp = f match {
    case FTrue => FTrue
    case Feat(_)     => f
    case FAnd(e1,e2) => FAnd(removeSS(e1),removeSS(e2))
    case FOr(e1,e2) => FOr(removeSS(e1),removeSS(e2))
    case FNot(e) => FNot(removeSS(e))
    case FImp(e1,e2) => FOr(FNot(removeSS(e1)), removeSS(e2))
    case FEq(e1,e2) => FAnd(removeSS(FImp(e1,e2)),removeSS(FImp(e2,e1)))
  }

  /**
    * Removes unreachable locations and edges
    * @param ifta
    * @return new IFTA without unreachable locations and edges
    */
  def removeUnreach(ifta: IFTA):IFTA = {
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
    IFTA(visited,ifta.init,ifta.act,ifta.clocks,ifta.feats,newedges,ifta.cInv.filter(i=> visited contains i._1),ifta.fm,ifta.in, ifta.out,ifta.aps.filter(n => visited contains n._1),ifta.shortname)
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
