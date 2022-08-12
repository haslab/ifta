package ifta.backend

import ifta._
import ifta.analyse.Simplify

/**
  * Created by guillecledou on 2019-05-17
  */


object CNF {

  // DANGER: concurrent executions of CNF must be protected by a lock to avoid sharing variables!
  private var vars: Map[FExp,Int] = Map()
  private var seed:Int = 1


  /**
    * Transforms a feature expression in CNF (in Dimacs format)
    * using Tseitin transformation.
    * (Shell for actual tseitinCNF)
    *
    * @param e feature expression
    * @return corresponding CNF in Dimacs format
    */
  def toCNFDimacs(e:FExp): List[List[Int]] = Simplify(Simplify.removeSS(e)) match {
    case FTrue => List()
    case FNot(FTrue) => List(List())
    case FNot(Feat(f)) => List(List(getVar(FNot(Feat(f)))))
    case Feat(f) => List(List(getVar(Feat(f))))
    case e1 => List(List(getVar(e1)))++tseitinCNF(e1)
  }

  def cnf2fexp(cnf:List[List[Int]]):List[FExp] = {
    var res = cnf.map(clause => clause.map(v => if (v>0) Feat("var_"+v) else FNot(Feat("var_"+(-v)))).fold(FNot(FTrue))(_||_))
    res.map(f=> Simplify(f))
  }

//  def toCNF(e:FExp):List[FExp] = {
//    def toFExp(v:Int):FExp = {
//      if (vars.contains(v))
//        if (v>0) vars(v) else FNot
//    }
//    val cnfDimacs = toCNFDimacs(e)
//    cnfDimacs.map(clause =>
//      clause.map(v =>
//        if (vars.contains(v)) )
//  }

  /**
    * Given a set of variables (Dimacs) that satisfy a feature expression
    * Return the actual name of the variables and their truth value
    * @param sol
    * @return a solution map - feature name -> Bool
    */
  def rebuildTseitinSol(sol:Set[Int]): Map[String,Boolean] = {
    var res: Map[String,Boolean] = Map()
    for((v,i) <- vars) {
      v match {
        case Feat(f) => {
          if (sol contains i) res += f -> true
          else if (sol contains (-i)) res += f -> false
        }
        case _ =>
      }
    }
    res
  }

  private def resetVars() = {vars = Map(); seed = 1 }

  private def getVar(e:FExp):Int = {
    var exp:FExp = e
    var sign:Int = 1
    e match {
      case FNot(e1) => {exp = e1; sign = -1}
      case _ =>
    }
    if (vars contains exp) vars(exp)*sign
    else {vars += exp -> seed; seed +=1; (seed-1)*sign}
  }


  // Two Notes on tseitin:
  // 1 - recursive cases can be improved
  // by checking first if the variable for the case exists,
  // in which case there is no need to call recursively
  // 2 - it could accept FImp and FEq
  // and simplify the formula in the moment

  /**
    * Recursive cases for creating a tseitin CNF
    *
    * @param e feature expression
    * @return tseitin CNF in Dimacs format
    */
  private def tseitinCNF(e:FExp):List[List[Int]] = e match{
    case FTrue => List()
    //case Feat(f) => List(List(gettVar(Feat(f))))
    case Feat(f) => List()
    case FNot(FTrue) => List(List())
    case FNot(e) => tseitinCNF(e)
    case FAnd(e1,e2) => tseitinCNF(e1) ++ tseitinCNF(e2) ++ tseitinAnd(e1,e2)
    case FOr(e1,e2) => tseitinCNF(e1) ++ tseitinCNF(e2) ++ tseitinOr(e1,e2)
    case _ =>
      throw new RuntimeException("Unexpected feat. expr. when generating CNF. Term needs to be simplified before conversion.")
  }

  /**
    * Creates a tseitin AND clause
    *
    * @param e1 frst feature expression for And clause
    * @param e2 snd feature expression for And clause
    * @return V <-> FAnd(e1,e2) in CNF Dimacs
    */
  private def tseitinAnd(e1:FExp,e2:FExp):List[List[Int]] = {
    val v = getVar(FAnd(e1,e2))
    val ve1 = getVar(e1)
    val ve2 = getVar(e2)
    List(List(-v,ve1),List(-v,ve2),List(v,-ve1,-ve2))
  }

  /**
    * Creates a tseitin OR clause
    *
    * @param e1 frst feature expression for Or clause
    * @param e2 snd feature expression for Or clause
    * @return V <-> FOr(e1,e2) in CNF Dimacs
    */
  private def tseitinOr(e1:FExp,e2:FExp):List[List[Int]] = {
    val v = getVar(FOr(e1,e2))
    val ve1 = getVar(e1)
    val ve2 = getVar(e2)
    List(List(v,-ve1),List(v,-ve2),List(-v,ve1,ve2))
  }

  // Just to check how it tseitinCNF works
  def showTseitin(cnf:List[List[Int]]):FExp ={
    var clauses:Set[FExp] =Set()
    var inverseMap:Map[Int,FExp] =Map()
    for ((f,i) <- vars) inverseMap += i -> f
    for (c <-cnf) {
      clauses += c.map(x => if (x>0) inverseMap(x) else FNot(inverseMap(-x))).fold(FNot(FTrue))(_||_)
    }
//    clauses.map(e => backend.Show(e)).mkString("","\n","")
    clauses.fold(FTrue)(_&&_)
  }

}
