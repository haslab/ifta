package ifta

/**
  * Created by jose on 30/09/16.
  */
sealed trait FExp {
  def &&(other:FExp) = FAnd(this,other)
  def ||(other:FExp) = FOr(this,other)
  def -->(other:FExp) = FNot(this) || other
}

case object FTrue                extends FExp
sealed abstract class Var(val name:String) extends FExp
case class Feat(n:String)     extends Var(n)
case class VN(n:String)       extends Var(n)
case class FAnd(e1:FExp,e2:FExp) extends FExp
case class FOr(e1:FExp,e2:FExp)  extends FExp
case class FNot(e:FExp)          extends FExp

