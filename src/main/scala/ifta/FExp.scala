package ifta

/**
  * Created by jose on 30/09/16.
  */
sealed trait FExp {
  def feats: List[String] = this match {
    case FTrue        => List()
    case v: Var       => List(v.name)
    case FAnd(e1, e2) => e1.feats ++ e2.feats
    case FOr(e1, e2)  => e1.feats ++ e2.feats
    case FNot(e)      => e.feats
  }
  def check(sol:Map[String,Boolean]): Boolean = this match {
    case FTrue        => true
    case v: Var       => sol.getOrElse(v.name,false) // elements not in the solution are considered false
    case FAnd(e1, e2) => e1.check(sol) && e2.check(sol)
    case FOr(e1, e2)  => e1.check(sol) || e2.check(sol)
    case FNot(e)      => !e.check(sol)
  }

  def &&(other:FExp) = FAnd(this,other)
  def ||(other:FExp) = FOr(this,other)
  def -->(other:FExp) = FNot(this) || other
  def <->(other:FExp) = (this --> other) && (other --> this)
}

case object FTrue                extends FExp
sealed abstract class Var(val name:String) extends FExp
case class Feat(n:String)     extends Var(n)
case class VN(n:String)       extends Var(n)
case class FAnd(e1:FExp,e2:FExp) extends FExp
case class FOr(e1:FExp,e2:FExp)  extends FExp
case class FNot(e:FExp)          extends FExp

