package ifta.analyse

import ifta._
import ifta.DSL._
import ifta.backend.Show
import ifta.reo.Connectors._
import preo.ast.{CPrim, CoreInterface}
import preo.frontend.mcrl2.{Action, Block, BoolDT, Channel, Comm, DataType, Hide, ITE, In, Init, LAnd, LIn, LNot, LTrue, Model, Nothing, Out, Par, PrimBuilder, Process, ProcessExpr, ProcessName, Seq, Sum}

/**
  * Created by guillecledou on 2019-05-08
  */


case class IftaModel(procs: List[Process], init: ProcessExpr)
  extends Model(procs,init) {

  override def toString: String = {
    val actions = procs.flatMap(p => p.getActions).filterNot(a => a.name.startsWith("fs")).mkString(",")
    val procsNames = procs.map(p => p.getName.name)

    val wrapProcesses = procs.filter(_.isInstanceOf[Channel]).map(c=>mkWrapProc(c.asInstanceOf[Channel]))


    val nifta = mkNifta(procs)
    val fm = Simplify(nifta.fm && mkFmSyncs(nifta,procs))
    val feats = nifta.iFTAs.flatMap(i => i.feats)

//    val inits = renameInitExpr(init)
//    val initsFsAct = getInitFsActNames(init,procs)

//    var initFsMap = getInitFsMap(procs)

    var processes = procs.map(p => if (p.isInstanceOf[Channel]) p.toString else renameInitSync(p.asInstanceOf[IftaInit]))

    var initsOrder:List[IftaInit] = processes.filter(p => p.isInstanceOf[IftaInit]).map(p => p.asInstanceOf[IftaInit]).sortBy(_.number)
    var fsMap: Map[ProcessName,(Action,Action,Action)] = Map()
    println("about to create newIftasInits")
    var newIftaInits = initsOrder.map(i => i match {
      case p@IftaInit(num,sa,fsa,List(c1,c2),hide) if c1.name.startsWith("Wrap") && c2.name.startsWith("Wrap")=>
        println(c1   + " ww   "+ c2)
        val fs1 = Action("fs"+c1.name.drop(4),Nothing,None)
        val fs2 = Action("fs"+c2.name.drop(4),Nothing,None)
        println(fs1 + "   " + fs2)
        fsMap+= p.getName -> (fs1,fs2,Action(fs1+"_"+fs2,Nothing,None))
        IftaInit(num,sa,(fs1,fs2),List(c1,c2),hide)
      case p@IftaInit(num,sa,fsa,List(c1,c2),hide) if c1.name.startsWith("Wrap") && c2.name.startsWith("Init") =>
        println(c1   + " wi   "+ c2)
        val fs1 = Action("fs"+c1.name.drop(4),Nothing,None)
        val fs2 = fsMap(c2)._3
        println(fs1 + "   " + fs2)
        fsMap+= p.getName -> (fs1,fs2,Action(fs1+"_"+fs2,Nothing,None))
        IftaInit(num,sa,(fs1,fs2),List(c1,c2),hide)
      case p@IftaInit(num,sa,fsa,List(c1,c2),hide) if c1.name.startsWith("Init") && c2.name.startsWith("Wrap") =>
        println(c1   + " iw   "+ c2)
        val fs1 = fsMap(c1)._3
        val fs2 = Action("fs"+c2.name.drop(4),Nothing,None)
        fsMap+= p.getName -> (fs1,fs2,Action(fs1+"_"+fs2,Nothing,None))
        println(fs1 + "   " + fs2)
        IftaInit(num,sa,(fs1,fs2),List(c1,c2),hide)
      case p@IftaInit(num,sa,fsa,List(c1,c2),hide) if c1.name.startsWith("Init") && c2.name.startsWith("Init") =>
        println(c1   + " ii   "+ c2)
        val fs1 = fsMap(c1)._3
        val fs2 = fsMap(c2)._3
        println(fs1 + "   " + fs2)
        fsMap+= p.getName -> (fs1,fs2,Action(fs1+"_"+fs2,Nothing,None))
        IftaInit(num,sa,(fs1,fs2),List(c1,c2),hide)
      case p@IftaInit(num,sa,fsa,List(c1),hide) if c1.name.startsWith("Init")  =>
        println(c1   + "  i  ")
        val fs1 = Action("",Nothing,None)
        val fs2 = Action("",Nothing,None)
        println(fs1 + "   " + fs2)
        fsMap+= p.getName -> (fs1,fs2,fsMap(c1)._3)
        IftaInit(num,sa,(fs1,fs2),List(c1),hide)
      case p@IftaInit(num,sa,fsa,List(c1),hide) if c1.name.startsWith("Wrap") =>
        println(c1   + "  w  ")
        val fs1 = Action("",Nothing,None)
        val fs2 = Action("",Nothing,None)
        println(fs1 + "   " + fs2)
        fsMap+= p.getName -> (fs1,fs2,Action("fs"+c1.name,Nothing,None))
        IftaInit(num,sa,(fs1,fs2),List(c1),hide)
    })

    println("created newIftasInits"+fsMap)

    processes = processes.filterNot(p => p.isInstanceOf[IftaInit]) ++ newIftaInits
    val fsActions = wrapProcesses.flatMap(_.getActions).map(_.toString.dropRight(4)) ++ newIftaInits.flatMap(p => p.getActions).filter(a => a.name.startsWith("fs"))
    val inits = renameInitExpr(init)
    val initsFsAct = getInitFsActNames(init,procs.filterNot(p => p.isInstanceOf[IftaInit]) ++ newIftaInits)
    val res = s"""
       |sort
       |  Feature = struct ${feats.map(f => s"$f(v:Bool)").mkString(" | ")};
       |  Product = List(Feature);
       |
       |act
       |  $actions;
       |  product,prod${if (fsActions.nonEmpty) fsActions.mkString(",",",","") else ""}:Product;
       |
       |proc
       |${mkFmProc(fm,feats)}

       |${processes.mkString("",";\n",";")}
       |${wrapProcesses.map(_.toStringNoRecursion).mkString("",";\n",";")}
       |
       |init
       |  block({prod,${initsFsAct.mkString(",")}},
       |    comm({prod | ${initsFsAct.mkString(" | ")} -> product},
       |      FM || $inits));
     """.stripMargin
//  FM || ${if (init.isInstanceOf[ProcessName] && init.asInstanceOf[ProcessName].name.startsWith("Init")) init else "Wrap"+init }));
    res
  }


//  private def remakeInits(inits:List[IftaInit]):List[IftaInit] = {
//    var fsMap: Map[ProcessName,Action] = Map()
//    inits.map(i =>
//      IftaInit(i.number,i.syncAct,)
//    )
//  }



  private def getInitFsMap(procs:List[Process]):Map[ProcessName,Action] = {
    procs.map(p => (p.getName -> getFsName(p,procs))).toMap
  }


  private def getFsName(proc:Process,procs:List[Process]):Action = proc match {
    case IftaInit(n,sa,fsa,pr,h) => Action(pr.map( p => getFsName(procs.find(a => a.getName == p ).get,procs).name).mkString("_"),Nothing,None)
    case c@Channel(name,_,_,_,_,_) => Action("fs"+c.getName.toString,Nothing,None)
  }

  private def getInitFsActNames(init:ProcessExpr,procs:List[Process]): Set[String] = {
    val procNames = init.getProcNames
    val initProcNames = procs.filter(p => procNames.contains(p.getName))
    var res:Set[String] = Set()
    res = initProcNames.flatMap(i => i match {
      case p@IftaInit(n,sa,fsa,pr,h) => Set(fsa._1+"_"+fsa._2)
      case c@Channel(_,_,_,_,_,_) => Set("fs"+c.getName.name.drop(4))
      case _ => Set[String]()
    }).toSet
    res
  }


  private def renameInitExpr(expr: ProcessExpr):ProcessExpr = expr match {
    case p@ProcessName(n,ap) if n.startsWith("Init") => p
    case ProcessName(n,ap) => ProcessName("Wrap"+n,ap)
    case Par(p1,p2) => Par(renameInitExpr(p1),renameInitExpr(p2))
    case Seq(p1,p2) => Seq(renameInitExpr(p1),renameInitExpr(p2))
    case Block(acts,in) => Block(acts,renameInitExpr(in))
    case Comm(s,in) => Comm(s,renameInitExpr(in))
    case Hide(acts,in) => Hide(acts,renameInitExpr(in))
    case Sum(vars,p) => Sum(vars,renameInitExpr(p))
    case ITE(c,t,None) => ITE(c,renameInitExpr(t))
    case ITE(c,t,Some(e)) => ITE(c,renameInitExpr(t),Some(renameInitExpr(e)))
  }

  private def renameInitSync(init:IftaInit):IftaInit = {
    IftaInit(init.number,init.syncAct,init.fsSyncAct,
      init.procs.map(p =>
        if (p.name.startsWith("Init"))
          p
        else ProcessName("Wrap"+p.name,p.actualParam)),
      init.toHide)
  }

//  private def remakeInitProcs(inits:List[Init]):List[Init] = {
//    var init2fsAction:Map[Init,Action] = Map()
//    for (init <- inits) {
//       var res = remakeInitProc(init,init2fsAction)
//        init2fsAction = res._2
//    }
//
//    List()
//  }
//
//  private def remakeInitProc(init:Init,init2fs:Map[Init,Action]):(IftaInit,Map[ProcessName,Action]) = init match {
//    case Init(num,a1,a2,procs,hide) =>
//      var fsActs = procs.map(pn => if (pn.name.startsWith("Wrap")) "fs"+pn.name.drop(4) else  )
//      var iftaInit = IftaInit(num,(a1,a2),  ,procs,hide)
//
//  }

  private def mkWrapProc(proc:Channel):Channel = {
    val act = Action("fs"+proc.getName,Nothing,None,List(("fs","Product")))
    Channel("Wrap" + proc.getName, None, List(act), List(), Sum(Map("fs" -> "Product"),act & proc.getNameWithActualParam),Map())
  }

  private def mkNifta(procs:List[Process]):NIFTA = {
    val iftas = procs.filter(_.isInstanceOf[Channel]).map(c => mkIfta(c.asInstanceOf[Channel]))
    NIFTA(iftas.toSet)
  }

  private def mkIfta(ch:Channel):IFTA = ch.name match {
    case "Dupl" => repl(ch.in.head.toString, ch.out.head.toString, ch.out.tail.map(o => o.toString).mkString(","))
    case "VDupl" => vrepl(ch.in.head.toString, ch.out.head.toString, ch.out.tail.map(o => o.toString).mkString(","))
    case "Merger" => merger(ch.in.head.toString,ch.in.tail.head.toString,(ch.in.tail.drop(1) ++ ch.out).map(a=> a.toString).mkString(","))
    case "VMerger" => vmerger(ch.in.head.toString,ch.in.tail.head.toString,(ch.in.tail.drop(1) ++ ch.out).map(a=> a.toString).mkString(","))
    case "Sync" => sync(ch.in.head.toString,ch.out.head.toString)
    case "Fifo" => fifo(ch.in.head.toString,ch.out.head.toString)
    case "Fifofull" => fifofull(ch.in.head.toString,ch.out.head.toString)
    case "Lossy" => lossy(ch.in.head.toString,ch.out.head.toString)
    case "Xor" => router(ch.in.head.toString,ch.out.head.toString, ch.out.tail.map(o => o.toString).mkString(","))
    case "VXor" => vrouter(ch.in.head.toString,ch.out.head.toString, ch.out.tail.map(o => o.toString).mkString(","))
    case "Drain" => sdrain(ch.in.head.toString,ch.in.last.toString)
    case "Writer" => writer(ch.out.head.toString)
    case "Reader" => reader(ch.in.head.toString)
    case "Timer" => timer(ch.in.head.toString,ch.out.head.toString,0) //todo:handle time properly
    case n if ch.in.size == 1 && ch.out.size == 1 => sync(ch.in.head.toString,ch.out.head.toString)
    case n => throw new RuntimeException(s"Unknown primitive ifta connector ${n}")
  }

  private def mkFmSyncs(nifta: NIFTA, processes: List[Process]):FExp = {
    val syncs = procs.map(p => p match {
      case i@IftaInit(_, _, _, _, _) => mkFmSync(nifta,i)
      case _ => FTrue
    })

    syncs.foldRight[FExp](FTrue)(_&&_)
  }

  private def mkFmSync(nifta:NIFTA,init:IftaInit):FExp = {
    val a1 = init.syncAct._1.toString
    val a2 = init.syncAct._2.toString
    val ifta1 = nifta.iFTAs.find(i => i.act.contains(a1))
    val ifta2 = nifta.iFTAs.find(i => i.act.contains(a2))
    (ifta1,ifta2) match {
      case (None,_) => FTrue
      case (_,None) => FTrue
      case (Some(ifta1),Some(ifta2)) =>ifta1.fe(a1) <-> ifta2.fe(a2)
    }
  }

  private def fm2mcrl2(fm:FExp):String = fm match {
    case FTrue => "true"
    case FNot(FTrue) => "false"
    case Feat(name) => "val_"+name
    case FAnd(e1, e2) => s"(${fm2mcrl2(e1)}) && (${fm2mcrl2(e2)})"
    case FOr(e1, e2) => s"(${fm2mcrl2(e1)}) || (${fm2mcrl2(e2)})"
    case FNot(e) => s"!(${fm2mcrl2(e)})"
    case FImp(e1, e2) => s"((${fm2mcrl2(e1)}) => (${fm2mcrl2(e2)}))"
    case FEq(e1, e2) =>
      val imp1 = fm2mcrl2(e1)
      val imp2 = fm2mcrl2(e2)
      s"(($imp1) => ($imp2)) && (($imp2) => ($imp1))"
  }

  private def mkFmProc(fm:FExp,feats:Set[String]):String = {
    val featsVal = feats.map(f => "val_"+f)
    s"""
       |FM = sum ${featsVal.mkString(",")}:Bool .
       |  (${fm2mcrl2(fm)}) -> prod(${feats.map(f => s"$f(val_$f)").mkString("[",",","]")});
     """.stripMargin
  }

}


case class IftaInit(number: Option[Int], syncAct:(Action,Action), fsSyncAct:(Action,Action), procs: List[ProcessName], var toHide: Boolean)
  extends Process{
  def getOperation: ProcessExpr = {
    val sync_action = syncAct._1 join syncAct._2
    val basicProc = procs.tail.foldRight(procs.head : ProcessExpr)((base, p) => Par(base, p))
    val operator =
      if (fsSyncAct._1.name!="")
        Block(List(syncAct._1, syncAct._2,fsSyncAct._1,fsSyncAct._2),
        Comm(List(
          (List(syncAct._1, syncAct._2), sync_action),
          (List(fsSyncAct._1,fsSyncAct._2),fsSyncAct._1 join fsSyncAct._2)),
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

  def getActions: Set[Action] = Set(syncAct._1 join syncAct._2, fsSyncAct._1 join fsSyncAct._2 )

  def getName: ProcessName= ProcessName(s"Init${if(number.isDefined) number.get else ""}")

  def toNumberedInit(n: Int): IftaInit = IftaInit(Some(n), syncAct, fsSyncAct, procs, toHide)
}


object IftaModel {

  implicit object IftaPrimBuilder extends PrimBuilder[IftaModel] {

    override def buildModel(proc: List[Process], init: ProcessExpr): IftaModel =
      IftaModel(proc,init)

    def buildPrimChannel(e: CPrim,chCount:Int): Channel = e match {
      case CPrim("sync",_,_,_) =>
        val in = Action("sync",In(1),Some(chCount))
        val out = Action("sync",Out(2),Some(chCount))
        val exp = ITE(LAnd(LIn(ActionFeat(in,LTrue),"fs"),LIn(ActionFeat(out,LTrue),"fs")),in | out)
        //        val exp = ITE(s"v_${in1}(true) in fs && v_${in2}(true) in fs",in1 | in2)
        Channel("Sync",Some(chCount),List(in),List(out), exp, Map("fs"->"Product"))
      case CPrim("drain",_,_,_) =>
        val in1 = Action("drain",In(1),Some(chCount))
        val in2 = Action("drain",In(2),Some(chCount))
        val exp = ITE(LAnd(LIn(ActionFeat(in1,LTrue),"fs"),LIn(ActionFeat(in2,LTrue),"fs")),in1 | in2)
//        val exp = ITE(s"v_${in1}(true) in fs && v_${in2}(true) in fs",in1 | in2)
        Channel("Drain",Some(chCount),List(in1,in2),List(), exp, Map("fs"->"Product"))
      case CPrim("fifo",_,_,_) =>
        val in = Action("fifo",In(1),Some(chCount))
        val out = Action("fifo",Out(1),Some(chCount))
//        val exp = ITE(s"v_${in}(true) in fs && v_${out}(true) in fs",in & out)
        val exp = ITE(LAnd(LIn(ActionFeat(in,LTrue),"fs"),LIn(ActionFeat(out,LTrue),"fs")),in & out)
        Channel("Fifo",Some(chCount),List(in),List(out), exp, Map("fs"->"Product"))
      case CPrim("fifofull",_,_,_) =>
        val in = Action("fifofull",In(1),Some(chCount))
        val out = Action("fifofull",Out(1),Some(chCount))
//        val exp = ITE(s"v_${in}(true) in fs && v_${out}(true) in fs",out & in)
        val exp = ITE(LIn(ActionFeat(in,LTrue),"fs") & LIn(ActionFeat(out,LTrue),"fs"),out & in)
        Channel("Fifofull",Some(chCount),List(in),List(out), exp, Map("fs"->"Product"))
      case CPrim("lossy",_,_,_) =>
        val in = Action("lossy",In(1),Some(chCount))
        val out = Action("lossy",Out(1),Some(chCount))
//        val exp = ITE(s"v_${in}(true) in fs && v_${out}(true) in fs",(in | out) + in)
        val exp = ITE(LIn(ActionFeat(in,LTrue),"fs") & LIn(ActionFeat(out,LTrue),"fs"),(in | out) + in)
        Channel("Lossy",Some(chCount),List(in),List(out), exp, Map("fs"->"Product"))
      case CPrim("dupl",_,_,_) =>
        val in = Action("dupl",In(1),Some(chCount))
        val out1 = Action("dupl",Out(1),Some(chCount))
        val out2 = Action("dupl",Out(2),Some(chCount))
//        val exp = ITE(s"v_${in}(true) in fs && v_${out1}(true) in fs && v_${out2}(true) in fs",in | out1 | out2 )
        val exp = ITE(LIn(ActionFeat(in,LTrue),"fs") & LIn(ActionFeat(out1,LTrue),"fs") & LIn(ActionFeat(out2,LTrue),"fs"),
          in | out1 | out2 )
        Channel("Dupl",Some(chCount),List(in),List(out1,out2), exp, Map("fs"->"Product"))
      case CPrim("vdupl",_,_,_) =>
        val in = Action("vdupl",In(1),Some(chCount))
        val out1 = Action("vdupl",Out(1),Some(chCount))
        val out2 = Action("vdupl",Out(2),Some(chCount))
//        val exp1 = ITE(s"v_${in.toString}(true) in fs && v_${out1}(true) in fs && v_${out2}(true) in fs",
//          in | out1 | out2 )
        val exp1 = ITE(LIn(ActionFeat(in,LTrue),"fs") & LIn(ActionFeat(out1,LTrue),"fs") & LIn(ActionFeat(out2,LTrue),"fs"),
            in | out1 | out2 )
//        val exp2 = ITE(s"v_${in.toString}(true) in fs && v_${out1}(true) in fs && !(v_${out2}(true) in fs)",
//          in | out1)
        val exp2 = ITE(LIn(ActionFeat(in,LTrue),"fs") & LIn(ActionFeat(out1,LTrue),"fs") & LNot(LIn(ActionFeat(out2,LTrue),"fs")),
            in | out1 )
//        val exp3 = ITE(s"v_${in.toString}(true) in fs && !(v_${out1}(true) in fs) && v_${out2}(true) in fs",
//          in | out2)
        val exp3 = ITE(LIn(ActionFeat(in,LTrue),"fs") & LNot(LIn(ActionFeat(out1,LTrue),"fs")) & LIn(ActionFeat(out2,LTrue),"fs"),
          in | out2 )
        Channel("VDupl",Some(chCount),List(in),List(out1,out2), exp1 + exp2 + exp3, Map("fs"->"Product"))
      case CPrim("xor",_,_,_) =>
        val in = Action("xor",In(1),Some(chCount))
        val out1 = Action("xor",Out(1),Some(chCount))
        val out2 = Action("xor",Out(2),Some(chCount))
        //        val exp = ITE(s"v_${in}(true) in fs && v_${out1}(true) in fs && v_${out2}(true) in fs",in | out1 | out2 )
        val exp = ITE(LIn(ActionFeat(in,LTrue),"fs") & LIn(ActionFeat(out1,LTrue),"fs") & LIn(ActionFeat(out2,LTrue),"fs"),
          (in | out1 ) + (in | out2))
        Channel("Xor",Some(chCount),List(in),List(out1,out2), exp, Map("fs"->"Product"))
      case CPrim("vxor",_,_,_) =>
        val in = Action("vxor",In(1),Some(chCount))
        val out1 = Action("vxor",Out(1),Some(chCount))
        val out2 = Action("vxor",Out(2),Some(chCount))
        val exp1 = ITE(LIn(ActionFeat(in,LTrue),"fs") & LIn(ActionFeat(out1,LTrue),"fs") & LIn(ActionFeat(out2,LTrue),"fs"),
          (in | out1) + (in | out2) )
        val exp2 = ITE(LIn(ActionFeat(in,LTrue),"fs") & LIn(ActionFeat(out1,LTrue),"fs") & LNot(LIn(ActionFeat(out2,LTrue),"fs")),
          in | out1 )
        val exp3 = ITE(LIn(ActionFeat(in,LTrue),"fs") & LNot(LIn(ActionFeat(out1,LTrue),"fs")) & LIn(ActionFeat(out2,LTrue),"fs"),
          in | out2 )
        Channel("VXor",Some(chCount),List(in),List(out1,out2), exp1 + exp2 + exp3, Map("fs"->"Product"))
      case CPrim("merger",_,_,_) =>
        val in1 = Action("merger",In(1),Some(chCount))
        val in2 = Action("merger",In(2),Some(chCount))
        val out = Action("merger",Out(1),Some(chCount))
//        val exp = ITE(s"v_${in1}(true) in fs && v_${in2}(true) in fs && v_${out}(true) in fs", in1 | in2 | out )
        val exp = ITE(LIn(ActionFeat(in1,LTrue),"fs") & LIn(ActionFeat(in2,LTrue),"fs") & LIn(ActionFeat(out,LTrue),"fs"),
          (in1 | out) + (in2 | out) )
        Channel("Merger",Some(chCount),List(in1,in2),List(out), exp, Map("fs"->"Product"))
      case CPrim("vmerger",_,_,_) =>
        val in1 = Action("vmerger",In(1),Some(chCount))
        val in2 = Action("vmerger",In(2),Some(chCount))
        val out = Action("vmerger",Out(1),Some(chCount))
//        val exp1 = ITE(s"v_${in1}(true) in fs && v_${in2}(true) in fs && v_${out}(true) in fs",
//          in1 | in2 | out )
        val exp1 = ITE(LIn(ActionFeat(in1,LTrue),"fs") & LIn(ActionFeat(in2,LTrue),"fs") & LIn(ActionFeat(out,LTrue),"fs"),
          (in1 | out) + (in2 | out) )
//        val exp2 = ITE(s"v_${in1}(true) in fs && !(v_${in2}(true) in fs) && v_${out}(true) in fs",
//          in1 | out )
        val exp2 = ITE(LIn(ActionFeat(in1,LTrue),"fs") & LNot(LIn(ActionFeat(in2,LTrue),"fs")) & LIn(ActionFeat(out,LTrue),"fs"),
          in1 | out )
//        val exp3 = ITE(s"!(v_${in1}(true) in fs) && v_${in2}(true) in fs && v_${out}(true) in fs",
//          in2 | out )
        val exp3 = ITE(LNot(LIn(ActionFeat(in1,LTrue),"fs")) & LIn(ActionFeat(in2,LTrue),"fs") & LIn(ActionFeat(out,LTrue),"fs"),
          in2 | out )
        Channel("VMerger",Some(chCount),List(in1,in2),List(out), exp1 + exp2 + exp3, Map("fs"->"Product"))
      case CPrim("writer",_,_,_) =>
        val out = Action("writer",Out(1),Some(chCount))
        val exp = ITE(LIn(ActionFeat(out,LTrue),"fs"),out)
        Channel("Writer",Some(chCount),List(),List(out), exp, Map("fs"->"Product"))
      case CPrim("reader",_,_,_) =>
        val in = Action("reader",In(1),Some(chCount))
        val exp = ITE(LIn(ActionFeat(in,LTrue),"fs"),in)
        Channel("Reader",Some(chCount),List(in),List(), exp, Map("fs"->"Product"))
      case CPrim("timer",_,_,extra) => //todo: handle time properly
        val in = Action("timer",In(1),Some(chCount))
        val out = Action("timer",Out(1),Some(chCount))
//        val exp = ITE(s"v_${in}(true) in fs && v_${out}(true) in fs",in & out)
        val exp = ITE(LAnd(LIn(ActionFeat(in,LTrue),"fs"),LIn(ActionFeat(out,LTrue),"fs")),in & out)
        Channel("Timer",Some(chCount),List(in),List(out), exp, Map("fs"->"Product"))

      case CPrim(name, CoreInterface(1), CoreInterface(1), _) =>
        val in = Action(name, In(1), Some(chCount))
        val out = Action(name, Out(1), Some(chCount))
        val exp = ITE(LAnd(LIn(ActionFeat(in,LTrue),"fs"),LIn(ActionFeat(out,LTrue),"fs")),in | out)
        val channel = Channel(number = Some(chCount), in = List(in), out = List(out),
          expression = exp,params = Map("fs"->"Product"))
        channel

      case CPrim(n,_,_,_) => throw new RuntimeException(s"Unknown connector ${n}")
    }

    def buildInits(names1: List[ProcessName], actions1: List[Action], names2: List[ProcessName], actions2: List[Action],initCount:Int):
    (Map[ProcessName, Process], Int) = {
      var map: Map[ProcessName, Process] = Map()
      var fsMap:Map[ProcessName,Action] = Map()
      var newCount = initCount

      actions1.zip(actions2).zip(names1.zip(names2)).foreach { case ((a1, a2), (n1, n2)) =>
        val real_name1 = getRealName(map, n1)
        val real_name2 = getRealName(map, n2)
        val fsa1:Action = if (real_name1.name.startsWith("Init")) {
          fsMap.getOrElse(real_name1,Action("fs"+real_name1.name,Nothing,None))
        }else Action("fs"+real_name1.name,Nothing,None)
        val fsa2:Action = if (real_name2.name.startsWith("Init")) {
          fsMap.getOrElse(real_name2,Action("fs"+real_name2.name,Nothing,None))
        } else Action("fs"+real_name2.name,Nothing,None)
        val init = IftaInit(Some(newCount), (a1, a2), (fsa1,fsa2),if(real_name1 != real_name2) List(real_name1, real_name2) else List(real_name1), false)

        map += (real_name1 -> init)
        map += (real_name2 -> init)
        fsMap += (init.getName -> Action(fsa1.name+"_"+fsa2.name,Nothing,None))
        newCount += 1
      }
      (map,newCount)
    }

    private def getRealName(map: Map[ProcessName, Process],name: ProcessName): ProcessName =
      if(map.contains(name)) getRealName(map, map(name).getName)
      else name
  }
}

case class ActionFeat(action:Action,value:BoolDT) extends DataType {
  override def toString: String = "v_"+action.toString + s"($value)"
}




