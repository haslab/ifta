package ifta.analyse

import ifta._

/**
  * Created by jose on 04/10/16.
  */
object Simplify {

  def apply(i:IFTA): IFTA =
    IFTA(i.locs,i.init,i.act,i.clocks,i.feats,i.edges.map(apply),i.cInv.mapValues(apply),apply(i.fm),i.vars,i.in,i.out)

  def apply(f:FExp): FExp = f match {
    case FTrue => FTrue
    case _: Var => f
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
      case (e1, e2) => FOr(e1, e2)
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
}
