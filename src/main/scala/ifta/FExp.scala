package ifta

/**
  * Created by jose on 30/09/16.
  */
sealed trait FExp

case object FTrue                extends FExp
sealed abstract class Var(name:String) extends FExp
case class Feat(name:String)     extends Var(name)
case class VN(name:String)       extends Var(name)
case class FAnd(e1:FExp,e2:FExp) extends FExp
case class FOr(e1:FExp,e2:FExp)  extends FExp
case class FNot(e:FExp)          extends FExp

