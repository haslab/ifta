package ifta

/**
  * Created by jose on 30/09/16.
  */
sealed trait FExp {
  def feats: List[String] = this match {
    case FTrue        => List()
    case Feat(name)   => List(name)
    case FAnd(e1, e2) => e1.feats ++ e2.feats
    case FOr(e1, e2)  => e1.feats ++ e2.feats
    case FNot(e)      => e.feats
    case FImp(e1, e2) => e1.feats ++ e2.feats
    case FEq(e1, e2)  => e1.feats ++ e2.feats
  }
  def check(sol:Map[String,Boolean]): Boolean = this match {
    case FTrue        => true
    case Feat(name)   => sol.getOrElse(name,false) // elements not in the solution are considered false
    case FAnd(e1, e2) => e1.check(sol) && e2.check(sol)
    case FOr(e1, e2)  => e1.check(sol) || e2.check(sol)
    case FNot(e)      => !e.check(sol)
    // removing syntactic sugar
    case FImp(e1, e2) => (FNot(e1)||e2).check(sol)
    case FEq(e1, e2)  => ((e1-->e2)&&(e2-->e1)).check(sol)
  }

  def &&(other:FExp) = FAnd(this,other)
  def ||(other:FExp) = FOr(this,other)
  def -->(other:FExp) = FImp(this,other) //FNot(this) || other
  def <->(other:FExp) = FEq(this,other)  //(this --> other) && (other --> this)
}

case object FTrue                extends FExp
case class Feat(n:String)        extends FExp
case class FAnd(e1:FExp,e2:FExp) extends FExp
case class FOr(e1:FExp,e2:FExp)  extends FExp
case class FNot(e:FExp)          extends FExp
// to simplify notation
case class FImp(e1:FExp,e2:FExp) extends FExp
case class FEq(e1:FExp,e2:FExp)  extends FExp

