package ifta.backend

import ifta._
import ifta.analyse.{Solver, Simplify}

/**
  * Created by jose on 03/10/16.
  * TODO: export IFTA as an UPPAAL automata
  */
object Uppaal {

  ///////
  // This are the one that really matters: for FTA and IFTA

  /**
    * build an UPPAAL automata from a network of FTAs
    *
    * @param tas
    * @return
    */
  def apply(tas: NFTA): String = {
    val auts = tas.fTAs.map(Simplify.apply)
    val acts = auts.flatMap(_.act)
    val sacts = acts.map(x => if (x.endsWith("!") || x.endsWith("?")) x.dropRight(1) else x)
    val feats = auts.flatMap(_.feats)
//    val fms = auts.map(_.fm).fold(FTrue)(_&&_)
//    println(s"auts ${auts.mkString("\n---\n")}")
    val fms1 = Simplify(auts.fold(FTA(Set(),0,Set(),Set(),Set(),Set(),Set(),Map(),FTrue))(_ mergeFM _).fm)
//    println(s"fms1: ${Show(fms1)}")
    val fms = if (feats.isEmpty) fms1
              else  fms1 && feats.foldLeft[FExp](FNot(FTrue))(_ || Feat(_)) // at least feat must hold in the uppaal model
//    println(s"solving ${Show(fms)}")
    s"""<?xml version="1.0" encoding="utf-8"?>
             |<!DOCTYPE nta PUBLIC '-//Uppaal Team//DTD Flat System 1.1//EN' 'http://www.it.uu.se/research/group/darts/uppaal/flat-1_2.dtd'>
             |<nta>
             |	<declaration>// Place global declarations here.
             |// Channels (actions)
             |${if (sacts.isEmpty) "" else sacts.mkString("chan ",",",";")}
             |// Features (booleans)
             |${if (feats.isEmpty) "" else feats.mkString("bool ",",",";")}
             | </declaration>
             |${auts.zipWithIndex.map(x=>mkTemplate(x._1,x._2)).mkString("\n")}
             |${mkContext(acts)}
             |${mkFeatModel(fms)}
             |<system>// Place template instantiations here.
             |//(no intantiation needed)
             |// List one or more processes to be composed into a system.
             |system ${if (auts.isEmpty) ""  else (0 until auts.size).map("FTA_"+_).mkString("",",",",")}
             |  ${if (auts.isEmpty) ";" else "Context, FeatModel;" }
             |</system>
             |	<queries>
             |	</queries>
             |</nta>
             |
        """.stripMargin
  }
//  def mkProcesses(fTA: FTA): String = {
//    s"""fTA${fTA.act.mkString("_", "_", "")} = FTA${fTA.act.mkString("_", "_", "")}();"""
//  }

  def mkContext(acts:Set[String]) = {
    s"""<template>
        |		<name x="5" y="5">Context</name>
        |		<declaration>
        |// Place local declarations here.
        |   </declaration>
        |   <location id="id0" x="0" y="0"> </location>
        |   <init ref="id0"/>"
        |${(for ((a,i) <- acts.zipWithIndex) yield mkCtxTrs(a,i,acts.size,acts)).mkString("\n")}
        |</template>
     """.stripMargin
  }
  private def mkCtxTrs(a:String,i:Int,total:Int,acts:Set[String]): String =
    if ((a.endsWith("!") && !acts.contains(a.dropRight(1)+"?"))
      ||(a.endsWith("?") && !acts.contains(a.dropRight(1)+"!")))
      s"""	<transition>
        |			<source ref="id0"/>
        |			<target ref="id0"/>
        |   	<label kind="synchronisation" x="76" y="${i*30-8-((total-1)*15)}">${
                if(a.endsWith("!")) a.dropRight(1)+"?" else a.dropRight(1)+"!"}</label>
        |			<nail x="68" y="-51"/>
        |			<nail x="68" y="42"/>
        |	 </transition>
      """.stripMargin
    else ""

  def mkFeatModel(fe:FExp) = {
    val sols = Solver.all(fe).zipWithIndex
    s"""<template>
        |		<name x="5" y="5">FeatModel</name>
        |		<declaration>
        |// Place local declarations here.
        |   </declaration>
        |// Locations
        |   <location id="id0" x="0" y="0"><committed/></location>
        |${(1 to sols.size).map(n =>
        "   <location id=\"id"+n+"\" x=\"100\" y=\""+place(n,sols.size)+"\"> </location>").mkString("\n")}
        |   <init ref="id0"/>"
        |// Transitions
        |${(for ((sol,n) <- sols) yield
        "   <transition> <source ref=\"id0\"/> <target ref=\"id"+(n+1)+"\"/> "+
           "<label kind=\"assignment\" x=\"130\" y=\"" + place(n+1, sols.size) + "\">"+
           sol.toIterable.map(p => if (p._2) p._1+"=true" else "").filter(_!="").mkString(",")+
           "</label> </transition>").mkString("\n")}
        | 	</template>
     """.stripMargin
  }
  private def place(n:Int,total:Int) =
    -(total-1)/2 * 50 + (50 * (n-1))

  def mkTemplate(fTA:FTA,index:Int):String = {
    s"""<template>
        |		<name x="5" y="5">FTA_$index</name>
        |		<declaration>
        |// Place local declarations 1 here.
        |// clocks:
        |${if (fTA.clocks.nonEmpty) fTA.clocks.mkString("clock ", ",", ";") else ""}
        |   </declaration>
        |// Locations
        |${fTA.locs.map(loc => mkLocation(loc, fTA.cInv.getOrElse(loc, CTrue),fTA.committed contains loc)).mkString("\n")}
        |   <init ref="id${fTA.init}"/>"
        |// Transitions
        |${fTA.edges.map(mkTransition).mkString("\n")}
        |	</template>
     """.stripMargin
  }

  private def mkTransition(e:FtaEdge): String = {
    s"""<transition>
        |  <source ref="id${e.from}"/>
        |  <target ref="id${e.to}"/>
        |  ${if (e.cCons==CTrue && e.fe==FTrue) "" else mkGuard(e.from,e.cCons,e.fe)}
        |  ${mkSyncLabel(e)}
        |  ${if (e.cReset.isEmpty) "" else mkReset(e.from,e.cReset)}
        |</transition>""".stripMargin
  }
  private def mkSyncLabel(e:FtaEdge): String =
    if (e.act.endsWith("!") || e.act.endsWith("?"))
      s"""<label kind="synchronisation" x="${e.from * 100 + 15}" y="-34">${e.act}</label>"""
    else ""

  /* e.g.
  <location id="id0" x="93" y="-8">
    <name x="83" y="-42">L2</name>
    <label kind="invariant" x="83" y="9">c &gt;= 2</label>
    <committed/> ... NOT IN IFTA YET
  </location>
 */
  private def mkLocation(loc:Int,cc:ClockCons,comm:Boolean): String = {
    s"""<location id="id$loc" x="${loc*100}" y="0">
        |<name x="${loc*100-10}" y="-34">L$loc</name>
        |${if (cc==CTrue) "" else mkInvariant(loc*100-10,cc)}
        |${if (comm) "<committed/>" else ""}
        |</location>""".stripMargin
  }

  private def mkInvariant(i:Int,cc:ClockCons): String =
    s"""<label kind="invariant" x="$i" y="11">${mkCC(cc)}</label>"""
  private def mkCC(cc:ClockCons): String = cc match {
    case CTrue => "true"
    case LT(c, n) => c+"&lt;"+n
    case GT(c, n) => c+"&gt;"+n
    case LE(c, n) => c+"&lt;="+n
    case GE(c, n) => c+"&gt;="+n
    case CAnd(cc1, cc2) => mkCC(cc1)+" &amp;&amp; "+mkCC(cc2)
  }
  private def mkFE(fe:FExp): String = fe match {
    case FTrue => "true"
    case Feat(name)   => name
    case FAnd(e1, e2) => mkFE(e1)+" &amp;&amp; "+mkFE(e2)
    case FOr(e1, e2) => mkFE(e1)+" || "+mkFE(e2)
    case FNot(e) => "not ("+mkFE(e)+")"
    case FImp(e1, e2) => mkFE(e1)+" &#8594; "+mkFE(e2)
    case FEq(e1, e2) => mkFE(e1)+" &#8596; "+mkFE(e2)
  }

  // e.g. <label kind="guard" x="18" y="-38">c &gt;= 5</label>
  private def mkGuard(from:Int,cc:ClockCons,fe:FExp): String =
    s"""<label kind="guard" x="${from*100+25}" y="-34">${mkCCFE(cc,fe)}</label>"""
  private def mkCCFE(cc:ClockCons,fe:FExp) =
    if      (cc==CTrue) mkFE(fe)
    else if (fe==FTrue) mkCC(cc)
    else mkCC(cc)+" &amp;&amp; "+mkFE(fe)
  // e.g. <label kind="assignment" x="18" y="-4">c:=0</label>
  private def mkReset(from:Int,as:Set[String]): String =
    s"""<label kind="assignment" x="${from*100+15}" y="-34">${as.map(_+":=0").mkString(", ")}</label>"""


///////////////////////////////
//  ////// OLDER EXPERIMENTS //
///////////////////////////////

//  def apply(n:NIFTA):String = {
//    val nIFTA = NIFTA((for (i <- n.iFTAs) yield Simplify(i)))
//    if (nIFTA.iFTAs.isEmpty) ""
//    else s"""<?xml version="1.0" encoding="utf-8"?>
//             |<!DOCTYPE nta PUBLIC '-//Uppaal Team//DTD Flat System 1.1//EN' 'http://www.it.uu.se/research/group/darts/uppaal/flat-1_2.dtd'>
//             |<nta>
//             |	<declaration>
//             | // Place global declarations here.
//             |${if (nIFTA.interfaces.nonEmpty) nIFTA.interfaces.mkString("chan ",",",";") else ""}
//             | </declaration>
//             |${nIFTA.iFTAs.map(i => mkTemplate(i)).mkString("\n")}
//             |<system>// Place template instantiations here.
//             |${nIFTA.iFTAs.map(i => mkProcesses(i)).mkString("\n")}
//             |// List one or more processes to be composed into a system.
//             |system ${nIFTA.iFTAs.map(i => i.act.mkString("iFTA_","_","") ).mkString(" ",",",";")}
//             |</system>
//             |	<queries>
//             |	</queries>
//             |</nta>
//             |
//      """.stripMargin
//  }
//
//  //  def apply(i: IFTA):String = {
//  //    val iFTA = Simplify(i)
//  //    s"""<?xml version="1.0" encoding="utf-8"?>
//  //      |<!DOCTYPE nta PUBLIC '-//Uppaal Teaem//DTD Flat System 1.1//EN' 'http://www.it.uu.se/research/group/darts/uppaal/flat-1_2.dtd'>
//  //      |<nta>
//  //      |	<declaration>
//  //      | // Place global declarations here.
//  //      | </declaration>
//  //      |	<template>
//  //      |		<name x="5" y="5">IFTA${iFTA.act.mkString("_","_","")}</name>
//  //      |		<declaration>
//  //      |// Place local declarations 1 here.
//  //      |// clocks:
//  //      |${if (iFTA.clocks.nonEmpty) iFTA.clocks.mkString("clock ",",",";") else ""}
//  //      |// channels (actions)
//  //      |${if (iFTA.act.nonEmpty) iFTA.act.mkString("chan ",",",";") else ""}
//  //      |   </declaration>
//  //      |// Locations
//  //      |${iFTA.locs.map(loc=>mkLocation(loc,iFTA.cInv.getOrElse(loc,CTrue))).mkString("\n")}
//  //      |${iFTA.init.headOption match {case Some(h)=>"   <init ref=\"id"+h+"\"/>";case None =>""}}
//  //      |// Transitions
//  //      |${iFTA.edges.map(mkTransition(_,iFTA.in,iFTA.out)).mkString("\n")}
//  //      |	</template>
//  //      |	<system>// Place template instantiations here.
//  //      |Process = IFTA${iFTA.act.mkString("_","_","")}();
//  //      |// List one or more processes to be composed into a system.
//  //      |system Process;
//  //      |    </system>
//  //      |	<queries>
//  //      |	</queries>
//  //      |</nta>
//  //      |
//  //    """.stripMargin
//  //  }
//
//  /* e.g.
//    iFTA_a_b = IFTA_a_b();
//   */
//  def mkProcesses(iFTA: IFTA): String = {
//    s"""iFTA${iFTA.act.mkString("_", "_", "")} = IFTA${iFTA.act.mkString("_", "_", "")}();"""
//  }
//
//  def mkTemplate(iFTA:IFTA):String = {
//    s"""<template>
//        |		<name x="5" y="5">IFTA${iFTA.act.mkString("_", "_", "")}</name>
//        |		<declaration>
//        |// Place local declarations 1 here.
//        |// clocks:
//        |${if (iFTA.clocks.nonEmpty) iFTA.clocks.mkString("clock ", ",", ";") else ""}
//        |// channels (actions)
//        |${if (iFTA.act.nonEmpty) iFTA.act.mkString("chan ", ",", ";") else ""}
//        |   </declaration>
//        |// Locations
//        |${iFTA.locs.map(loc => mkLocation(loc, iFTA.cInv.getOrElse(loc, CTrue))).mkString("\n")}
//        |   <init ref="id${iFTA.init}"/>"
//        |// Transitions
//        |${iFTA.edges.map(mkTransition(_, iFTA.in, iFTA.out)).mkString("\n")}
//        |	</template>
//     """.stripMargin
//  }
//  //${iFTA.init.headOption match {case Some(h) => "   <init ref=\"id" + h + "\"/>"; case None => ""}}
//
//
//  /* e.g.
//  	<transition>
//			<source ref="id1"/>
//			<target ref="id0"/>
//			<label kind="guard" x="18" y="-38">c &gt;= 5 &amp;&amp; c&lt;=2 &amp;&amp; c&gt;1</label>
//			<label kind="synchronisation" x="18" y="-21">a</label>
//			<label kind="assignment" x="18" y="-4">c:=0</label>
//		</transition>
//		keep only edges with in and out actions, and add ! or ?
//   */
//  private def mkTransition(e:Edge,in:Set[String],out:Set[String]): String = {
//    s"""<transition>
//        |  <source ref="id${e.from}"/>
//        |  <target ref="id${e.to}"/>
//        |  ${if (e.cCons==CTrue || e.fe==FTrue) "" else mkGuard(e.from,e.cCons,e.fe)}
//        |  ${if (e.act.size!=1) "" else mkSync(e.from,e.act.head,in,out)}
//        |  ${if (e.cReset.isEmpty) "" else mkReset(e.from,e.cReset)}
//        |</transition>""".stripMargin
//  }
//  // e.g. <label kind="synchronisation" x="18" y="-21">a!</label>
//  private def mkSync(from:Int,a:String,in:Set[String],out:Set[String]): String = {
//    if (in contains a)
//      s"""<label kind="synchronisation" x="${from * 100 + 15}" y="-34">$a?</label>"""
//    else if (out contains a)
//      s"""<label kind="synchronisation" x="${from * 100 + 15}" y="-34">$a!</label>"""
//    else ""
//  }

}