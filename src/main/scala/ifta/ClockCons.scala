package ifta

/**
  * Created by jose on 30/09/16.
  */
sealed trait ClockCons {

  // probably to remove, after fixing "flatten2" in NIFTA
  def restrict(clocks: Set[String]): ClockCons = this match {
    case LT(c, n) if !(clocks contains c) => CTrue
    case GT(c, n) if !(clocks contains c) => CTrue
    case LE(c, n) if !(clocks contains c) => CTrue
    case GE(c, n) if !(clocks contains c) => CTrue
    case CAnd(cc1, cc2) => (cc1.restrict(clocks),cc2.restrict(clocks)) match {
      case (CTrue,cc3) => cc3
      case (cc3,CTrue) => cc3
      case (cc3,cc4) => CAnd(cc3,cc4)
    }
    case _ => this
  }

  def clocks: List[String] = this match {
    case CTrue => List()
    case LT(c, n) => List(c)
    case GT(c, n) => List(c)
    case LE(c, n) => List(c)
    case GE(c, n) => List(c)
    case CAnd(cc1, cc2) => cc1.clocks ++ cc2.clocks
  }

  def &(other:ClockCons) = CAnd(this,other)
}

case object CTrue                extends ClockCons
case class LT(c:String,n:Int)  extends ClockCons
case class GT(c:String,n:Int)  extends ClockCons
case class LE(c:String,n:Int)  extends ClockCons
case class GE(c:String,n:Int)  extends ClockCons
case class CAnd(cc1:ClockCons,cc2:ClockCons) extends ClockCons


