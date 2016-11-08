package ifta

import ifta.backend.Show

/**
  * Created by jose on 11/10/16.
  */
case class FTA(locs:Set[Int], init:Int, committed:Set[Int], act:Set[String], clocks:Set[String]
               , feats:Set[String], edges:Set[FtaEdge], cInv:Map[Int,ClockCons], fm:FExp, aps:Map[Int,String]) {
  override def toString = Show(this)

  /**
    * returns a new FTA that properly merges the features, feature models, and actions, nothing else.
    * @param other
    * @return
    */
  def mergeFM(other:FTA): FTA = {
    val shared = act.map(dropSuf) intersect other.act.map(dropSuf)
    val resFM: FExp = (fm && other.fm) &&
      (for (a <- shared) yield fPort(a) <-> other.fPort(a)).fold(FTrue)(_&&_)
    FTA(Set(),0,Set(),act++other.act,Set(),feats++other.feats,edges++other.edges,Map(),resFM,Map())
  }

  def fPort(a:String): FExp =
    (for ( e <- edges; if dropSuf(e.act) == a) yield e.fe).fold(FNot(FTrue))(_||_)
  private def dropSuf(s:String):String =
    if (s.endsWith("!") || s.endsWith("?")) s.dropRight(1) else s

}

case class FtaEdge(from:Int, cCons: ClockCons, act:String
                   ,cReset:Set[String], fe:FExp, to:Int)

case class NFTA(fTAs:Set[FTA]) {
  // constructors
  def ||(i:FTA):NFTA = NFTA(fTAs+i)
  def ||(n:NFTA):NFTA = NFTA(fTAs++n.fTAs)

  override def toString = fTAs.map(Show.apply).mkString(" ||\n")
}
