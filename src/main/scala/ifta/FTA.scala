package ifta

import ifta.backend.Show

/**
  * Created by jose on 11/10/16.
  */
case class FTA(locs:Set[Int], init:Int, committed:Set[Int], act:Set[String], clocks:Set[String]
               , feats:Set[String], edges:Set[FtaEdge], cInv:Map[Int,ClockCons], fm:FExp) {
  override def toString = Show(this)
}

case class FtaEdge(from:Int, cCons: ClockCons, act:String
                   ,cReset:Set[String], fe:FExp, to:Int)

