package ifta

/**
  * Created by jose on 14/11/16.
  */

import org.scalatest.junit.JUnitSuite
import org.junit.Test
import org.junit.Assert._
import DSL._
import examples.CoffeeMachine._

class TestCoffeeMachine {

  @Test def testProdAlt(): Unit = {
    assertEquals("rename then compose = product of parallel",aut2a, aut2b)
    assertEquals("sync(l) then product() = product(l)",aut2b, aut2c)
    assertEquals("flatten = product()",aut2c, aut2d)
  }
}
