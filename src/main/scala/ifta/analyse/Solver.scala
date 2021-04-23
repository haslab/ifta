package ifta.analyse

import ifta._
import ifta.backend.CNF
import org.sat4j.core.VecInt
import org.sat4j.minisat.SolverFactory
import org.sat4j.specs.ISolver
import org.sat4j.tools.ModelIterator


/**
  * Created by jose on 03/10/16.
  * Uses SAT4J to check if a FExp is valid.
  */
object Solver {
  /**
    * Solve a feature expression, by searching for a mapping
    * from features and variable names to booleans that satisfy "e"
    *
    * @param e expression to be solved
    * @return [[None]] if there is no solution, or [[Some(sol)]] if a solution exists
    */
  def apply(e:FExp): Option[Map[String,Boolean]] = {
//    resetVars()
    //val cnf = toCNF(e)
    val cnf = CNF.toCNFDimacs(Simplify(Simplify.removeSS(e))) // remove syntactic sugar and simplify first
//    println("vars: "+vars.mkString(","))
//    println("CNF: "+cnf)
    val sol = solve(cnf)
//    println("sol: "+sol)

//    sol.map(x=>rebuildSol(x.toSet))
    sol.map(x=>CNF.rebuildTseitinSol(x.toSet))
  }

//  private def rebuildTseitinSol(sol:Set[Int]): Map[String,Boolean] = {
//    var res: Map[String,Boolean] = Map()
//    for((v,i) <- vars) {
//      v match {
//        case Feat(f) => {
//          if (sol contains i) res += f -> true
//          else if (sol contains (-i)) res += f -> false
//        }
//        case _ =>
//      }
//    }
//    res
//  }

  def all(e:FExp): List[Map[String,Boolean]] = {
//    resetVars()
//    val cnf = toCNF(e)
//    val sols = solveAll(cnf)
//    sols.map(x=>rebuildSol(x.toSet))
//    resetVars()
    val cnf = CNF.toCNFDimacs(Simplify(Simplify.removeSS(e)))
    val sols = solveAll(cnf)
    sols.map(x=> CNF.rebuildTseitinSol(x.toSet))
  }

//  private var vars:Map[String,Int] = Map()
//  private var seed:Int = 1
//  private def resetVars() = {vars = Map(); seed = 1}
//  private def getVar(v:String): Int =
//    if (vars contains v) vars(v)
//    else { vars += v -> seed; seed += 1; seed-1 }
//
//  private def toCNF(e:FExp): List[List[Int]] = e match {
//    case FTrue => List()
//    case Feat(name)   => List(List(getVar(name)))
//    case FAnd(e1, e2) => toCNF(e1) ++ toCNF(e2)
//    case FOr(e1, e2) =>
//      for (c1 <- toCNF(e1); c2<-toCNF(e2))
//        yield c1 ++ c2
//    case FNot(ne) => ne match {
//      case FTrue => List(List())
//      case Feat(name)   => List(List(-getVar(name)))
//      case FAnd(e1, e2) => toCNF(FOr(FNot(e1),FNot(e2)))
//      case FOr(e1, e2) => toCNF(FAnd(FNot(e1),FNot(e2)))
//      case FNot(nne) => toCNF(nne)
//      case FImp(e1,e2) => toCNF(FNot(FNot(e1) || e2))
//      case FEq(e1,e2) => toCNF(FNot((e1-->e2) && (e2-->e1)))
//    }
//    // removing syntactic sugar
//    case FImp(e1,e2) => toCNF(FNot(e1) || e2)
//    case FEq(e1,e2) => toCNF((e1-->e2) && (e2-->e1))
//  }

//  var vars: Map[FExp,Int] = Map()
//  var seed:Int = 1
//  def resetVars() = {vars = Map(); seed = 1 }
//  def gettVar(e:FExp):Int =
//    if (tvars contains e) tvars(e)
//    else {tvars += e -> tseed; tseed +=1; tseed-1}

//  def getVar(e:FExp):Int = {
//    var exp:FExp = e
//    var sign:Int = 1
//    e match {
//      case FNot(e1) => {exp = e1; sign = -1}
//      case _ =>
//    }
//    if (vars contains exp) vars(exp)*sign
//    else {vars += exp -> seed; seed +=1; (seed-1)*sign}
//  }

//  /**
//    * Transforms a feature expression in CNF
//    * using Tseitin transformation.
//    * (Shell for actual tseitinCNF)
//    *
//    * @param e feature expression
//    * @return corresponding CNF in Dimacs format
//    */
//  def toCNF(e:FExp): List[List[Int]] = e match {
//    case FTrue => List()
//    case FNot(FTrue) => List(List())
//    case FNot(Feat(f)) => List(List(getVar(FNot(Feat(f)))))
//    case Feat(f) => List(List(getVar(Feat(f))))
//    case e1 => List(List(getVar(e1)))++tseitinCNF(e1)
//  }
//
//  // Two Notes on tseitin:
//  // 1 - recursive cases can be improved
//  // by checking first if the variable for the case exists,
//  // in which case there is no need to call recursively
//  // 2 - it could accept FImp and FEq
//  // and simplify the formula in the moment
//
//  /**
//    * Recursive cases for creating a tseitin CNF
//    *
//    * @param e feature expression
//    * @return tseitin CNF in Dimacs format
//    */
//  private def tseitinCNF(e:FExp):List[List[Int]] = e match{
//    case FTrue => List()
//    //case Feat(f) => List(List(gettVar(Feat(f))))
//    case Feat(f) => List()
//    case FNot(FTrue) => List(List())
//    case FNot(e) => tseitinCNF(e)
//    case FAnd(e1,e2) => tseitinCNF(e1) ++ tseitinCNF(e2) ++ tseitinAnd(e1,e2)
//    case FOr(e1,e2) => tseitinCNF(e1) ++ tseitinCNF(e2) ++ tseitinOr(e1,e2)
//  }
//
//  /**
//    * Creates a tseitin AND clause
//    *
//    * @param e1 frst feature expression for And clause
//    * @param e2 snd feature expression for And clause
//    * @return V <-> FAnd(e1,e2) in CNF Dimacs
//    */
//  private def tseitinAnd(e1:FExp,e2:FExp):List[List[Int]] = {
//    val v = getVar(FAnd(e1,e2))
//    val ve1 = getVar(e1)
//    val ve2 = getVar(e2)
//    List(List(-v,ve1),List(-v,ve2),List(v,-ve1,-ve2))
//  }
//
//  /**
//    * Creates a tseitin OR clause
//    *
//    * @param e1 frst feature expression for Or clause
//    * @param e2 snd feature expression for Or clause
//    * @return V <-> FOr(e1,e2) in CNF Dimacs
//    */
//  private def tseitinOr(e1:FExp,e2:FExp):List[List[Int]] = {
//    val v = getVar(FOr(e1,e2))
//    val ve1 = getVar(e1)
//    val ve2 = getVar(e2)
//    List(List(v,-ve1),List(v,-ve2),List(-v,ve1,ve2))
//  }
//
//  // Just to check how it tseitinCNF works
//  def showTseitin(cnf:List[List[Int]]):String ={
//    var clauses:Set[FExp] =Set()
//    var inverseMap:Map[Int,FExp] =Map()
//    for ((f,i) <- vars) inverseMap += i -> f
//    for (c <-cnf) {
//      clauses += c.map(x => if (x>0) inverseMap(x) else FNot(inverseMap(-x))).fold(FNot(FTrue))(_||_)
//    }
//    clauses.map(e => backend.Show(e)).mkString("","\n","")
//  }

  //  def -->(other:FExp) = FImp(this,other) //FNot(this) || other
//  def <->(other:FExp) = FEq(this,other)  //(this --> other) && (other --> this)


//  private def rebuildSol(sol:Set[Int]): Map[String,Boolean] = {
//    var res: Map[String,Boolean] = Map()
//    for((v,i) <- vars)
//      if (sol contains i) res += v -> true
//      else if (sol contains (-i)) res += v -> false
//    res
//  }

  private def solve(cnf:List[List[Int]]): Option[Array[Int]] = {
    val solver:ISolver = SolverFactory.newDefault()
    for (c <- cnf)
      solver.addClause(new VecInt(c.toArray))
    if (solver.isSatisfiable)
      Some(solver.findModel())
    else None
  }

  private def solveAll(cnf:List[List[Int]]): List[Array[Int]] = {
    val solver:ISolver = SolverFactory.newDefault()
    val mi: ModelIterator = new ModelIterator(solver)
    var res:List[Array[Int]] = List()
    for (c <- cnf)
      solver.addClause(new VecInt(c.toArray))
    while(mi.isSatisfiable) {
//      println(mi.numberOfModelsFoundSoFar())
      val m = mi.model()
      res ::= m
    }
    res
  }

//  ISolver solver = SolverFactory.newDefault();
//  ModelIterator mi = new ModelIterator(solver);
//  solver.setTimeout(3600); // 1 hour timeout
//  Reader reader = new InstanceReader(mi);
//
//  // filename is given on the command line
//  try {
//    boolean unsat = true;
//    IProblem problem = reader.parseInstance(args[0]);
//    while (problem.isSatisfiable()) {
//      unsat = false;
//      int [] model = problem.model();
//      // do something with each model
//    }



//    // just to experiment with the constraint solver
//  def main(args: Array[String]) {
//    import scala.language.implicitConversions
//    implicit def toFeat(s:String): Feat = Feat(s)
//
//    def test(fExp: FExp): Unit =
//      println(s"testing $fExp - ${Solver(fExp)}")
//
//    test("a" --> "b")
//    test(("a" && "b") || ("b" && "c") )
//    test(("a" || "b") && ("b" || "c") )
//    test(("a" || "b") && FNot("a") && FNot("b") )
//
//    //    val MAXVAR: Int = 1000000
////    val NBCLAUSES: Int = 500000
//
////    val solver:ISolver = SolverFactory.newDefault()
//
//    // prepare the solver to accept MAXVAR variables. MANDATORY for MAXSAT solving
////    solver.newVar(MAXVAR)
////    solver.setExpectedNumberOfClauses(NBCLAUSES)
//    // Feed the solver using Dimacs format, using arrays of int
//    // (best option to avoid dependencies on SAT4J IVecInt)
////    solver.addClause(new VecInt(List[Int](1,-3).toArray)) // "a \/ not c"
//
////    for (i <- 0 until NBCLAUSES) {
////      int [] clause = // get the clause from somewhere
////        // the clause should not contain a 0, only integer (positive or negative)
////        // with absolute values less or equal to MAXVAR
////        // e.g. int [] clause = {1, -3, 7}; is fine
////        // while int [] clause = {1, -3, 7, 0}; is not fine
////        solver.addClause(new VecInt(clause)); // adapt Array to IVecInt
////    }
//
//    // we are done. Working now on the IProblem interface
////    val problem: IProblem  = solver
////    println("problem: "+problem+" (done)")
////    if (problem.isSatisfiable()) {
////      println("sol: "+problem.findModel().mkString(","))
////    } else {
////      println("sol not found")
////    }
//
//  }

}
