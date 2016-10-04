package ifta.backend

import ifta._
import ifta.analyse.Simplify

/**
  * Created by jose on 03/10/16.
  */
object Show {
  def apply(fExp: FExp): String = Simplify(fExp) match {
    case FTrue => "true"
    case v: Var => v.name
//    case FAnd(e1, e2@FAnd(_,_)) => parShow(e1)+" && "+Show(e2)
    case FAnd(FAnd(e1,e2), e3) => parShow(e1)+" && "+apply(FAnd(e2,e3))
    case FAnd(e1, e2) => parShow(e1)+" && "+parShow(e2)
    case FOr(FOr(e1,e2), e3) => parShow(e1)+" || "+apply(FOr(e2,e3))
    case FOr(e1, e2) => parShow(e1)+" || "+parShow(e2)
    case FNot(e) => "!"+parShow(e)
  }
  private def parShow(fExp: FExp): String = fExp match {
    case FTrue | _:Var | FNot(_) => Show(fExp)
    case _ => "("+apply(fExp)+")"
  }

  def apply(clockCons: ClockCons): String = clockCons match {
    case CTrue => "true"
    case LT(c, n) => s"$c < $n"
    case GT(c, n) => s"$c > $n"
    case LE(c, n) => s"$c <= $n"
    case GE(c, n) => s"$c >= $n"
    case CAnd(cc1, cc2) => s"${apply(cc1)} & ${apply(cc2)}"
  }
}
