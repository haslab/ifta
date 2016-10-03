package ifta

import ifta.analyse.Solver
import org.scalatest.junit.JUnitSuite
import org.junit.Test
import org.junit.Assert._
import DSL._

/**
  * Created by jose on 03/10/16.
  */
class TestSolver {

  def hasSol(fExp: FExp): Unit =
      assert(Solver(fExp).isDefined, s"$fExp has solutions")
//    println(s"testing $fExp - ${Solver(fExp)}")

  def fails(fExp: FExp) =
    assert(Solver(fExp).isEmpty, s"$fExp has no solutions")

  @Test def runTests() {
    hasSol("a" --> "b")
    hasSol(("a" && "b") || ("b" && "c") )
    hasSol(("a" || "b") && ("b" || "c") )
    fails(("a" || "b") && not("a") && not("b") )
  }

}
