package ifta

/**
  * Created by jose on 30/09/16.
  */
sealed trait ClockCons {
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
case class LT(c:String,n:Float)  extends ClockCons
case class GT(c:String,n:Float)  extends ClockCons
case class LE(c:String,n:Float)  extends ClockCons
case class GE(c:String,n:Float)  extends ClockCons
case class CAnd(cc1:ClockCons,cc2:ClockCons) extends ClockCons


