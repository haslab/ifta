package ifta

import ifta.analyse.{Simplify, Solver}

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

  /**
    * Checks if a given instantiation of features satisfies the feature expression
    * @param sol instantiation of features
    * @return
    */
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

  /**
    * Calculates the set of products allowed by the feature expression
    * w.r.t a set of features
    * @param fts set of features
    * @return the set of all valid feature selections, i.e., a set of valid products
    */
  def products(fts:Set[String]):Set[Set[String]] = {
    val fExp = Simplify(this)
    val ftsNotUsed = fts -- fExp.feats.toSet
    val fm =
      if (ftsNotUsed.isEmpty) fExp
      else  FAnd(fExp,(ftsNotUsed.foldLeft[FExp](FNot(FTrue))(_ || Feat(_)) || Feat("__feat__")))
    val sols = Solver.all(Simplify(fm))
    val allSols = (for (so <- sols) yield so.map(s => if (s._2) s._1 else "").toSet.filterNot(_ =="")).toSet
    allSols.map(s => s.filterNot( _ =="__feat__"))
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

