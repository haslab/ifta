package ifta.backend

import ifta._
import ifta.analyse.Simplify

/**
  * Created by jose on 03/10/16.
  */
object Show {
  def apply(fExp: FExp): String = showFExp(Simplify(fExp))
  def showFExp(fExp: FExp): String = fExp match {
    case FTrue => "true"
    case Feat(name) => name
//    case FAnd(e1, e2@FAnd(_,_)) => parShow(e1)+" && "+Show(e2)
    case FAnd(FAnd(e1,e2), e3) => parShow(e1)+" && "+showFExp(FAnd(e2,e3))
    case FAnd(e1, e2) => parShow(e1)+" && "+parShow(e2)
    case FOr(FOr(e1,e2), e3) => parShow(e1)+" || "+showFExp(FOr(e2,e3))
    case FOr(e1, e2) => parShow(e1)+" || "+parShow(e2)
    case FNot(e) => "!"+parShow(e)
    case FImp(e1, e2) => parShow(e1)+" --> "+parShow(e2)
    case FEq(e1, e2) => parShow(e1)+" <-> "+parShow(e2)
  }
  private def parShow(fExp: FExp): String = fExp match {
    case FTrue | Feat(_) | FNot(_) => showFExp(fExp)
    case _ => "("+showFExp(fExp)+")"
  }

  def apply(clockCons: ClockCons): String = showCC(Simplify(clockCons))
  def showCC(clockCons: ClockCons): String = clockCons match {
    case CTrue => "true"
    case LT(c, n) => s"$c < $n"
    case GT(c, n) => s"$c > $n"
    case LE(c, n) => s"$c <= $n"
    case GE(c, n) => s"$c >= $n"
    case CAnd(cc1, cc2) => s"${showCC(cc1)} & ${showCC(cc2)}"
  }

  def apply(i: IFTA): String = {
    val iFTA = Simplify.remUnreach(i)
    s"""IFTA [${iFTA.init}${(iFTA.locs - iFTA.init).mkString("|", ",", "")}] """ +
      (if (iFTA.act.isEmpty) "" else s"""${iFTA.act.mkString("[", ",", "]")} """) +
      (if (iFTA.clocks.isEmpty) "" else s"""${iFTA.clocks.mkString("[", ",", "]")} """) +
      (if (iFTA.feats.isEmpty) "" else s"""${iFTA.feats.mkString("[", ",", "]")} """) +
      (if (iFTA.in.isEmpty && iFTA.out.isEmpty) ""
      else
        s"""${iFTA.in.mkString("[", ",", "]")}->${iFTA.out.mkString("[", ",", "]")} """) +
      (if (Simplify(iFTA.fm) == FTrue) "" else s"\n  ${Show(iFTA.fm)}") +
      iFTA.edges.map(x => "\n  " + Show(x)).mkString("")
  }

  def apply(e:Edge): String =
    s"""${e.from} --> ${e.to} ${mbAct(e.act)}${mbCC(e.cCons)}${mbCReset(e.cReset)}${mbFE(e.fe)}"""
  private def mbAct(as:Set[String]) = if (as.isEmpty) "" else "by "+as.mkString(",")+" "
  private def mbAct(a:String) = "by "+a+" "
  private def mbCC(cc:ClockCons) = if (cc == CTrue) "" else "cc "+Show(cc)+" "
  private def mbCReset(res:Set[String]) = if (res.isEmpty) "" else "reset "+res.mkString(",")+" "
  private def mbFE(fe:FExp) = if (fe == FTrue) "" else "when "+Show(fe)+" "


  def apply(fTA: FTA): String =
    s"""FTA [${fTA.init}${((fTA.locs--fTA.committed)-fTA.init).mkString("|",",","")}${(fTA.committed-fTA.init).mkString("|",",","")}] """+
      (if (fTA.act.isEmpty) "" else s"""${fTA.act.mkString("[",",","]")} """)+
      (if (fTA.clocks.isEmpty) "" else s"""${fTA.clocks.mkString("[",",","]")} """)+
      (if (fTA.feats.isEmpty) "" else s"""${fTA.feats.mkString("[",",","]")} """)+
      (if (Simplify(fTA.fm) == FTrue) "" else s"""${Show(fTA.fm)}""")+
      fTA.edges.map(x=>"\n  "+Show(x)).mkString("")

  def apply(e:FtaEdge): String =
    s"""${e.from} --> ${e.to} ${mbAct(e.act)}${mbCC(e.cCons)}${mbFE(e.fe)}"""

}
