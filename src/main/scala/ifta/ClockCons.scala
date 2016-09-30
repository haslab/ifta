package ifta

/**
  * Created by jose on 30/09/16.
  */
sealed trait ClockCons

case object CTrue                extends ClockCons
case class LT(c:String,n:Float)  extends ClockCons
case class GT(c:String,n:Float)  extends ClockCons
case class LE(c:String,n:Float)  extends ClockCons
case class GE(c:String,n:Float)  extends ClockCons
case class CAnd(cc1:ClockCons,cc2:ClockCons) extends ClockCons


