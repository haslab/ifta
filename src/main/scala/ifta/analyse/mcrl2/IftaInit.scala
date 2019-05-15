package ifta.analyse.mcrl2

import preo.frontend.mcrl2._

/**
  * Created by guillerminacledou on 2019-05-15
  */

case class IftaInit(number: Option[Int], syncAct:(Action,Action), fsSyncAct:Option[(Action,Action)], procs: List[ProcessName], var toHide: Boolean)
  extends Process{
  def getOperation: ProcessExpr = {
    val sync_action = syncAct._1 join syncAct._2
    val basicProc = procs.tail.foldRight(procs.head : ProcessExpr)((base, p) => Par(base, p))
    val operator =
      if (fsSyncAct.isDefined)
        Block(List(syncAct._1, syncAct._2,fsSyncAct.get._1,fsSyncAct.get._2),
          Comm(List(
            (List(syncAct._1, syncAct._2), sync_action),
            (List(fsSyncAct.get._1,fsSyncAct.get._2),fsSyncAct.get._1 join fsSyncAct.get._2)),
            basicProc))
      else
        Block(List(syncAct._1, syncAct._2),
          Comm(List(
            (List(syncAct._1, syncAct._2), sync_action)),
            basicProc))

    if(toHide) Hide(List(sync_action), operator)
    else operator
  }

  override def toString: String = s"Init${if(number.isDefined) number.get else ""} = ${getOperation.toString}"

  def getActions: Set[Action] =
    Set(syncAct._1 join syncAct._2) ++
      (if (fsSyncAct.isDefined) Set(fsSyncAct.get._1 join fsSyncAct.get._2 ) else Set())

  def getName: ProcessName= ProcessName(s"Init${if(number.isDefined) number.get else ""}")

  def toNumberedInit(n: Int): IftaInit = IftaInit(Some(n), syncAct, fsSyncAct, procs, toHide)
}
