package ifta.analyse.mcrl2

import preo.frontend.mcrl2.{Action, BoolDT, DataType}

/**
  * Created by guillecledou on 2019-05-15
  */

case class ActionFeat(action:Action,value:BoolDT) extends DataType {
  override def toString: String = "v_"+action.toString + s"($value)"
}
