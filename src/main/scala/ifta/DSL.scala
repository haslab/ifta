package ifta

import java.io.{FileWriter, BufferedWriter}

import ifta.analyse.IFTA2FTA
import ifta.analyse.Simplify

object DSL {
  // to help building clock constraints
  class CVar(n:String) {
    def <(f:Int): ClockCons = LT(n,f)
    def >(f:Int): ClockCons = GT(n,f)
    def <=(f:Int): ClockCons = LE(n,f)
    def >=(f:Int): ClockCons = GE(n,f)
  }

  class ELoc(i:Int) {
    def -->(other:Int): Edge = Edge(i,true,Set(),Set(),true,other)
  }

  implicit def toNIFTA(iFTA:IFTA): NIFTA = NIFTA(Set(iFTA))
  implicit def toFeat(s:String): Feat = Feat(s)
  implicit def toCVar(s:String): CVar = new CVar(s)
  implicit def boolToCC(b:Boolean): ClockCons =
    if (b) CTrue else throw new RuntimeException("clock constraints cannot be false")
  implicit def boolToFE(b:Boolean): FExp =
    if (b) FTrue else FNot(FTrue)
  implicit def intToELoc(i:Int): ELoc = new ELoc(i)

  def not(fExp: FExp) = FNot(fExp)
  val newifta = IFTA(Set(0),0,Set(),Set(),Set(),Set(),Map(),true,Set(),Set())
//  val nifta = NIFTA(Set())

  val toDot = ifta.backend.Dot
  def con2dot(nIFTA: NIFTA) = ifta.backend.Dot.connector(nIFTA)

  def toFTA = IFTA2FTA

  // NIFTA -> NFTA -> UPPAAL
  def toUppaal(nIFTA: NIFTA) = backend.Uppaal(IFTA2FTA(nIFTA))
  def toUppaal(nIFTA: NIFTA, file:String) = {
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(backend.Uppaal(IFTA2FTA(nIFTA))) // NIFTA -> NFTA -> UPPAAL
    bw.close()
  }

  ////////////////////
  // Reo connectors //
  ////////////////////

  def v(s:String) = "v"+s

//  def router(i:String,o1:String,o2:String) = newifta ++ (
//    0 --> 0 by s"$i,$o1" when v(i) && v(o1),
//    0 --> 0 by s"$i,$o2" when v(i) && v(o2),
//    0 --> 0 by s"$i" when (v(i) && not(v(o1) || v(o2))) // can receive if there are no outputs
//    ) get i pub s"$o1,$o2" when (v(o1) || v(o2)) --> v(i)

  def router(i:String,o1:String,o2:String, outs:String*): IFTA = {
    val nouts = Set(o1,o2) ++ outs.toSet
    var ifta = newifta ++ (0 --> 0 by i when v(i) && not(mkFOr(mkFeat(nouts))))
    for ( o <- nouts )
      ifta ++= (0 --> 0 by Set(i,o) when v(i) && v(o))
    ifta get i pub nouts.mkString(",") when (mkFOr(mkFeat(nouts))) --> v(i)
  }

  def fifo(i:String,o:String) = newifta ++ (
    0 --> 1 by s"$i" when v(i) && v(o),
    1 --> 0 by s"$o" when v(i) && v(o),
    0 --> 0 by s"$i" when v(i) && not(v(o))  // can receive if there is no output
    ) get i pub o   when v(o) --> v(i)

  def sync(i:String,o:String) = newifta ++ (
    0 --> 0 by s"$i,$o" when v(i) && v(o),
    0 --> 0 by s"$i" when v(i) && not(v(o))  // can receive if there is no output
    //    0 --> 0 by s"$o" when not(v(i)) && v(o) // drop: cannot produce if there is no input
    ) get i pub o  when v(o) --> v(i)

  def sdrain(i:String,i2:String) = newifta ++ (
    0 --> 0 by s"$i,$i2" when v(i) && v(i2),
    0 --> 0 by s"$i" when v(i) && not(v(i2)),
    0 --> 0 by s"$i2" when v(i2) && not(v(i))
    ) get i get i2

  def asdrain(i:String,i2:String) = newifta ++ (
    0 --> 0 by s"$i" when v(i),
    0 --> 0 by s"$i2" when v(i2)
    ) get i get i2

//  def join(i:String,i2:String,o:String) = newifta ++ (
//    0 --> 0 by s"$i,$i2,$o" when v(i) && v(i2) && v(o),     // default: all ports
//    0 --> 0 by s"$i2,$o" when not(v(i)) && v(i2) && v(o),   // can behave as sync if only one input and the output are present
//    0 --> 0 by s"$i2" when not(v(i)) && v(i2) && not(v(o)), // can behave as asyncdrain if only one input is present
//    0 --> 0 by s"$i,$i2" when v(i) && v(i2) && not(v(o)),   // can behave as syncdrain if only the inputs are present
//    0 --> 0 by s"$i,$o" when v(i) && not(v(i2)) && v(o),    // can behave as sync if only one input and the output is present
//    0 --> 0 by s"$i" when v(i) && not(v(i2)) && not(v(o)) // can behave as asyncdrain if only one input is present
//    ) get i get i2 pub o  when v(o) --> (v(i) || v(i2))

  def join(i:String,i2:String,inOrO:String,rest:String*) = {
    val o = if (rest.isEmpty) inOrO else rest.last
    var ifta = newifta
    val ins = if (!rest.isEmpty) Set(i,i2,inOrO) ++ (rest.toSet - o) else Set(i,i2)
    val ss = (ins + o).subsets().toSet - Set() - Set(o)
    for (s <- ss)
      ifta ++= (0 --> 0 by s when mkFAnd(mkFeat(s)) && not(mkFOr(mkFeat((ins + o) -- s))))
    ifta get ins.mkString(",") pub o when v(o) --> mkFOr(mkFeat(ins))
  }

//  def merger(i:String, i2:String, o:String) = newifta ++ (
//    0 --> 0 by s"$i,$o" when v(i) && v(o),    // default
//    0 --> 0 by s"$i" when v(i) && not(v(o)),  // can behave as asyncdrain if there is no output
//    0 --> 0 by s"$i2,$o" when v(i2) && v(o),  // default
//    0 --> 0 by s"$i2" when v(i2) && not(v(o)) // can behave as asyncdrain if there is no output
//    ) get i get i2 pub o  when v(o) --> (v(i) || v(i2))

  def merger(i:String, i2:String, inOrO:String, rest:String*):IFTA ={
    val o = if (rest.isEmpty) inOrO else rest.last
    var ifta = newifta
    val ins = if (!rest.isEmpty) Set(i,i2,inOrO) ++ (rest.toSet - o) else Set(i,i2)
    for (i <- ins)
      ifta ++= (
        0 --> 0 by Set(i,o) when v(i) && v(o),
        0 --> 0 by Set(i) when v(i) && not(v(o))
        )
    ifta get ins.mkString(",") pub o when v(o) --> mkFOr(mkFeat(ins))
  }

//  def repl(i:String,o1:String,o2:String) = newifta ++ (
//    0 --> 0 by s"$i,$o1,$o2" when v(i) && v(o1) && v(o2), //default: all ports
//    0 --> 0 by s"$i,$o1" when v(i) && v(o1) && not(v(o2)),              // can behave as sync if only one output is present
//    0 --> 0 by s"$i,$o2" when v(i) && not(v(o1)) && v(o2),              // can behave as sync if only one output is present
//    0 --> 0 by s"$i" when v(i) && not(v(o1) || v(o2))     // can behave as asyncdrain if only the input is present
//    ) get i pub o1 pub o2  when (v(o1) || v(o2)) --> v(i)

  def repl(i:String,o1:String,o2:String,outs:String*):IFTA = {
    val nouts = Set(o1,o2) ++ outs.toSet
    var ifta = newifta
    for (ss <- nouts.subsets())
      ifta ++= (0 --> 0 by Set(i)++ss when v(i) && mkFAnd(mkFeat(ss)) && not(mkFOr(mkFeat(nouts -- ss)))) // all possible set of outputs + input
    ifta get i pub nouts.mkString(",") when (mkFOr(mkFeat(nouts))) --> v(i)
  }

  private def mkFeat(act:Set[String]):Set[FExp] = act.map(a => Feat(v(a)))

  private def mkFAnd(feats:Set[FExp]):FExp = Simplify(feats.fold(FTrue)(_&&_))

  private def mkFOr(feats:Set[FExp]):FExp = Simplify(feats.fold(FNot(FTrue))(_||_))
}
