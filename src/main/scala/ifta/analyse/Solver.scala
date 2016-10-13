package ifta.analyse

import ifta._
import org.sat4j.core.VecInt
import org.sat4j.minisat.SolverFactory
import org.sat4j.specs.{IProblem, ISolver}
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
    resetVars()
    val cnf = toCNF(e)
//    println("vars: "+vars.mkString(","))
//    println("CNF: "+cnf)
    val sol = solve(cnf)
//    println("sol: "+sol)
    sol.map(x=>rebuildSol(x.toSet))
  }

  def all(e:FExp): List[Map[String,Boolean]] = {
    resetVars()
    val cnf = toCNF(e)
    val sols = solveAll(cnf)
    sols.map(x=>rebuildSol(x.toSet))
  }

  private var vars:Map[String,Int] = Map()
  private var seed:Int = 1
  private def resetVars() = {vars = Map(); seed = 1}
  private def getVar(v:String): Int =
    if (vars contains v) vars(v)
    else { vars += v -> seed; seed += 1; seed-1 }

  private def toCNF(e:FExp): List[List[Int]] = e match {
    case FTrue => List()
    case Feat(name)   => List(List(getVar(name)))
    case FAnd(e1, e2) => toCNF(e1) ++ toCNF(e2)
    case FOr(e1, e2) =>
      for (c1 <- toCNF(e1); c2<-toCNF(e2))
        yield c1 ++ c2
    case FNot(ne) => ne match {
      case FTrue => List(List())
      case Feat(name)   => List(List(-getVar(name)))
      case FAnd(e1, e2) => toCNF(FOr(FNot(e1),FNot(e2)))
      case FOr(e1, e2) => toCNF(FAnd(FNot(e1),FNot(e2)))
      case FNot(nne) => toCNF(nne)
    }
  }

  private def rebuildSol(sol:Set[Int]): Map[String,Boolean] = {
    var res: Map[String,Boolean] = Map()
    for((v,i) <- vars)
      if (sol contains i) res += v -> true
      else if (sol contains (-i)) res += v -> false
    res
  }

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



    // just to experiment with the constraint solver
  def main(args: Array[String]) {
    implicit def toFeat(s:String): Feat = Feat(s)

    def test(fExp: FExp) =
      println(s"testing $fExp - ${Solver(fExp)}")

    test("a" --> "b")
    test(("a" && "b") || ("b" && "c") )
    test(("a" || "b") && ("b" || "c") )
    test(("a" || "b") && FNot("a") && FNot("b") )

    //    val MAXVAR: Int = 1000000
//    val NBCLAUSES: Int = 500000

//    val solver:ISolver = SolverFactory.newDefault()

    // prepare the solver to accept MAXVAR variables. MANDATORY for MAXSAT solving
//    solver.newVar(MAXVAR)
//    solver.setExpectedNumberOfClauses(NBCLAUSES)
    // Feed the solver using Dimacs format, using arrays of int
    // (best option to avoid dependencies on SAT4J IVecInt)
//    solver.addClause(new VecInt(List[Int](1,-3).toArray)) // "a \/ not c"

//    for (i <- 0 until NBCLAUSES) {
//      int [] clause = // get the clause from somewhere
//        // the clause should not contain a 0, only integer (positive or negative)
//        // with absolute values less or equal to MAXVAR
//        // e.g. int [] clause = {1, -3, 7}; is fine
//        // while int [] clause = {1, -3, 7, 0}; is not fine
//        solver.addClause(new VecInt(clause)); // adapt Array to IVecInt
//    }

    // we are done. Working now on the IProblem interface
//    val problem: IProblem  = solver
//    println("problem: "+problem+" (done)")
//    if (problem.isSatisfiable()) {
//      println("sol: "+problem.findModel().mkString(","))
//    } else {
//      println("sol not found")
//    }

  }

}
