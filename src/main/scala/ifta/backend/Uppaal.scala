package ifta.backend

import ifta._
import ifta.analyse.Simplify

/**
  * Created by jose on 03/10/16.
  * TODO: export IFTA as an UPPAAL automata
  */
object Uppaal {

  def apply(n:NIFTA):String = {
    val nIFTA = NIFTA((for (i <- n.iFTAs) yield Simplify(i)))
    if (nIFTA.iFTAs.isEmpty) ""
    else s"""<?xml version="1.0" encoding="utf-8"?>
        |<!DOCTYPE nta PUBLIC '-//Uppaal Team//DTD Flat System 1.1//EN' 'http://www.it.uu.se/research/group/darts/uppaal/flat-1_2.dtd'>
        |<nta>
        |	<declaration>
        | // Place global declarations here.
        |${if (nIFTA.interfaces.nonEmpty) nIFTA.interfaces.mkString("chan ",",",";") else ""}
        | </declaration>
        |${nIFTA.iFTAs.map(i => mkTemplate(i)).mkString("\n")}
        |<system>// Place template instantiations here.
        |${nIFTA.iFTAs.map(i => mkProcesses(i)).mkString("\n")}
        |// List one or more processes to be composed into a system.
        |system ${nIFTA.iFTAs.map(i => i.act.mkString("iFTA_","_","") ).mkString(" ",",",";")}
        |</system>
        |	<queries>
        |	</queries>
        |</nta>
        |
      """.stripMargin
    }

//  def apply(i: IFTA):String = {
//    val iFTA = Simplify(i)
//    s"""<?xml version="1.0" encoding="utf-8"?>
//      |<!DOCTYPE nta PUBLIC '-//Uppaal Teaem//DTD Flat System 1.1//EN' 'http://www.it.uu.se/research/group/darts/uppaal/flat-1_2.dtd'>
//      |<nta>
//      |	<declaration>
//      | // Place global declarations here.
//      | </declaration>
//      |	<template>
//      |		<name x="5" y="5">IFTA${iFTA.act.mkString("_","_","")}</name>
//      |		<declaration>
//      |// Place local declarations 1 here.
//      |// clocks:
//      |${if (iFTA.clocks.nonEmpty) iFTA.clocks.mkString("clock ",",",";") else ""}
//      |// channels (actions)
//      |${if (iFTA.act.nonEmpty) iFTA.act.mkString("chan ",",",";") else ""}
//      |   </declaration>
//      |// Locations
//      |${iFTA.locs.map(loc=>mkLocation(loc,iFTA.cInv.getOrElse(loc,CTrue))).mkString("\n")}
//      |${iFTA.init.headOption match {case Some(h)=>"   <init ref=\"id"+h+"\"/>";case None =>""}}
//      |// Transitions
//      |${iFTA.edges.map(mkTransition(_,iFTA.in,iFTA.out)).mkString("\n")}
//      |	</template>
//      |	<system>// Place template instantiations here.
//      |Process = IFTA${iFTA.act.mkString("_","_","")}();
//      |// List one or more processes to be composed into a system.
//      |system Process;
//      |    </system>
//      |	<queries>
//      |	</queries>
//      |</nta>
//      |
//    """.stripMargin
//  }

  /* e.g.
    iFTA_a_b = IFTA_a_b();
   */
  def mkProcesses(iFTA: IFTA): String = {
    s"""iFTA${iFTA.act.mkString("_", "_", "")} = IFTA${iFTA.act.mkString("_", "_", "")}();"""
  }

  def mkTemplate(iFTA:IFTA):String = {
    s"""<template>
        |		<name x="5" y="5">IFTA${iFTA.act.mkString("_", "_", "")}</name>
        |		<declaration>
        |// Place local declarations 1 here.
        |// clocks:
        |${if (iFTA.clocks.nonEmpty) iFTA.clocks.mkString("clock ", ",", ";") else ""}
        |// channels (actions)
        |${if (iFTA.act.nonEmpty) iFTA.act.mkString("chan ", ",", ";") else ""}
        |   </declaration>
        |// Locations
        |${iFTA.locs.map(loc => mkLocation(loc, iFTA.cInv.getOrElse(loc, CTrue))).mkString("\n")}
        |${iFTA.init.headOption match {case Some(h) => "   <init ref=\"id" + h + "\"/>"; case None => ""}}
        |// Transitions
        |${iFTA.edges.map(mkTransition(_, iFTA.in, iFTA.out)).mkString("\n")}
        |	</template>
     """.stripMargin
  }

  /* e.g.
  	<location id="id0" x="93" y="-8">
			<name x="83" y="-42">L2</name>
			<label kind="invariant" x="83" y="9">c &gt;= 2</label>
			<committed/> ... NOT IN IFTA YET
		</location>
   */
  private def mkLocation(loc:Int,cc:ClockCons): String = {
    s"""<location id="id$loc" x="${loc*100}" y="0">
       |<name x="${loc*100-10}" y="-34">L$loc</name>
       |${if (cc==CTrue) "" else mkInvariant(loc*100-10,cc)}
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

  /* e.g.
  	<transition>
			<source ref="id1"/>
			<target ref="id0"/>
			<label kind="guard" x="18" y="-38">c &gt;= 5 &amp;&amp; c&lt;=2 &amp;&amp; c&gt;1</label>
			<label kind="synchronisation" x="18" y="-21">a</label>
			<label kind="assignment" x="18" y="-4">c:=0</label>
		</transition>
		keep only edges with in and out actions, and add ! or ?
   */
  private def mkTransition(e:Edge,in:Set[String],out:Set[String]): String = {
    s"""<transition>
      |  <source ref="id${e.from}"/>
      |  <target ref="id${e.to}"/>
      |  ${if (e.cCons==CTrue) "" else mkGuard(e.from,e.cCons)}
      |  ${if (e.act.size!=1) "" else mkSync(e.from,e.act.head,in,out)}
      |  ${if (e.cReset.isEmpty) "" else mkReset(e.from,e.cReset)}
      |</transition>""".stripMargin
  }

  // e.g. <label kind="guard" x="18" y="-38">c &gt;= 5</label>
  private def mkGuard(from:Int,cc:ClockCons): String =
    s"""<label kind="guard" x="${from*100+25}" y="-34">${mkCC(cc)}</label>"""
  // e.g. <label kind="synchronisation" x="18" y="-21">a!</label>
  private def mkSync(from:Int,a:String,in:Set[String],out:Set[String]): String = {
    if (in contains a)
      s"""<label kind="synchronisation" x="${from * 100 + 15}" y="-34">$a?</label>"""
    else if (out contains a)
      s"""<label kind="synchronisation" x="${from * 100 + 15}" y="-34">$a!</label>"""
    else ""
  }
  // e.g. <label kind="assignment" x="18" y="-4">c:=0</label>
  private def mkReset(from:Int,as:Set[String]): String =
    s"""<label kind="assignment" x="${from*100+15}" y="-34">${as.map(_+":=0").mkString(", ")}</label>"""
}