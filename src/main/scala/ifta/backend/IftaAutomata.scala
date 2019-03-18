package ifta.backend

import ifta.analyse.Simplify
import ifta._
import ifta.DSL._
import ifta.reo.Connectors._
import preo.ast.CPrim
import preo.backend.{Automata, AutomataBuilder, Network}
import preo.backend.Network.Prim
import preo.common.GenerationException



/**
  * Created by guillecledou on 13/03/2019
  *
  * To be used only from ReoLanguage/ReoLive
  */

// TODO: only build the automata in this class when need, not while using join
/**
  * A new representation of an Ifta, aimed at being generated from a [[preo.backend.Network]],
  * and to reuse functionality meant for [[Automata]]
  * @param ports all possible actions as ints
  * @param init initial state
  * @param trans
  */
case class IftaAutomata(ifta:IFTA,nifta:Set[IFTA],conns:Set[Prim]) extends Automata {

  private var inSeed: Int = 0
  private var outSeed: Int = 0
  private var internalSeed: Int = 0

  private lazy val portName: Map[String, String] =
    (ifta.in ++ ifta.out).map(p => p -> mkPortName(p)).toMap

  private lazy val internalNames: Map[String,String] =
    (nifta.flatMap(n => n.act) -- (ifta.in ++ ifta.out)).zipWithIndex.map(m => (m._1 -> m._2.toString)).toMap

  /** Set of states of the automata, represented as integers */
  override def getStates: Set[Int] = ifta.locs

  /** Returns the initial state */
  override def getInit: Int = ifta.init

  /** Returns the transitions to be displayed */
  override def getTrans(fullName: Boolean): Set[(Int, Any, String, Int)] =
    for (e <- ifta.edges) yield (
      e.from
      , (Simplify(e.cCons) match {
          case CTrue => ""
          case cc => Show(cc)}) + "~"
      + e.act.map(p => getPortName(p)+getDir(p)).mkString(".") + "~"
      + Show(Simplify(getRenamedFe(e.fe))) + "~"
      + e.cReset.map(c => s"$c = 0").mkString(",")
      , (e.from, e.to, e.act, e.fe, e.cCons, e.cReset).hashCode().toString()
      , e.to
    )

  /** An easier to read representation */
  override def show: String = Show(Simplify(ifta))

  /** Return the fm with feats renamed after ports (or internal ports)*/
  def getFm:FExp = getRenamedFe(Simplify(ifta.fm))

  /** Return feats renamed after ports (or internal ports)*/
  def getFeats:Set[FExp] = ifta.feats.map(f => getRenamedFe(Feat(f)))

  /**
    * Returns the fancy name of an interface port
    *
    * @param p
    * @return
    */
  def getPortName(p: String): String = {
    if ((ifta.in ++ ifta.out).contains(p))
      portName.getOrElse(p, p.toString)
    else
      throw new RuntimeException(s"Unknown port, ${p}, for this automaton")
  }

  /**
    * Makes a fancy name for an interface port
    * if the port is associated to an edge that is named by the user (e.g., put1), it uses such a name
    * otherwise it uses a name a generic name inX for inputs and outX for outputs,
    * where X is an index identifying different ins and outs
    *
    * @param p
    * @return
    */
  private def mkPortName(p: String): String = {
    var name = ""
    // conn from which p comes - since only in out are kept, there is only one conn
    var conn =
      if (ifta.in.contains(p))
        conns.find(c => c.ins.map(_.toString).contains(p))
      else
        conns.find(c => c.outs.map(_.toString).contains(p))
    if (conn.nonEmpty) {
      if (preo.DSL.PRIMITIVE.contains(conn.get.prim.name))
        name = getPortIndexedName(p)
      else
        name = conn.get.prim.name
    } else throw new RuntimeException("Port not found in IftaAutomata when assigning new name")
    name
  }

  /**
    * Creates a unique name for an interface port
    * inX or outX dependin if p is an input or an output,
    * X is a unique seed to identify different ins and outs
    *
    * @param p
    * @return
    */
  private def getPortIndexedName(p: String): String = {
    if (ifta.in.contains(p)) {
      inSeed += 1
      s"in${if (ifta.in.size > 1) inSeed else ""}"
    } else if (ifta.out.contains(p)) {
      outSeed += 1
      s"out${if (ifta.out.size > 1) outSeed else ""}"
    } else ""
  }

  /**
    * Returns the direction of an interface port,
    * Since it is an interface port, it is either an input or an output
    * @param p
    * @return
    */
  private def getDir(p:String):String =
    if (ifta.in.contains(p))
      "↓"
    else if (ifta.out.contains(p))
      "↑"
    else ""

  /** Rename a fexp so that features associated to port have the ports name - instead of e.g. v_0*/
  def getRenamedFe(fe:FExp):FExp = fe match {
    case Feat(n)      =>
      if (n.startsWith("v_")) {
        var name = n.slice(2, n.size)
        Feat(portName.getOrElse(name, "f"+internalNames.getOrElse(name,n)))
      } else Feat(n)
    case FTrue        => FTrue
    case FAnd(e1, e2) => getRenamedFe(e1) && getRenamedFe(e2)
    case FOr(e1, e2)  => getRenamedFe(e1) || getRenamedFe(e2)
    case FNot(e)      => FNot(getRenamedFe(e))
    case FImp(e1,e2)  => getRenamedFe(e1) --> getRenamedFe(e2)
    case FEq(e1,e2)   => getRenamedFe(e1) <-> getRenamedFe(e2)
  }

}


object IftaAutomata {

  // from -> (target, ports, clockcons, fe, originalEdge)
  type Trans = Set[(Int,(Int,Set[Int],ClockCons, FExp,Set[Prim]))]

  implicit object IftaAutomataBuilder extends AutomataBuilder[IftaAutomata] {

    /**
      * Translate Prim connectors into IftaAutomata
      * @param e a [[Prim]] connector
      * @param seed is ignored. Here just to complied with the interface declaration, not needed.
      * @return an IftaAutomata and a seed (just 0, not used)
      */
    def buildAutomata(e: Prim, seed: Int): (IftaAutomata,Int) = {
      var iFta:IFTA =  e match {
        case Prim(CPrim("sync",_,_,_),List(a),List(b),_) =>
          sync(a.toString,b.toString) name "sync"
        case Prim(CPrim("id",_,_,_),List(a),List(b),_) =>
          sync(a.toString,b.toString) name "id"
        case Prim(CPrim("lossy", _, _, _), List(a), List(b),_) =>
          lossy(a.toString,b.toString) name "lossy"
        case Prim(CPrim("fifo",_,_,_),List(a),List(b),_) =>
          fifo(a.toString,b.toString) name "fifo"
        case Prim(CPrim("fifofull", _, _, _), List(a), List(b),_) =>
          fifofull(a.toString,b.toString) name "fifofull"
        case Prim(CPrim("drain", _, _, _), List(a, b), List(),_) =>
          sdrain(a.toString,b.toString) name "drain"
        case Prim(CPrim("merger", _, _, _), List(a, b), List(c),_) =>
          merger(a.toString,b.toString,c.toString) name "merger"
        case Prim(CPrim("vmerger", _, _, _), List(a, b), List(c),_) =>
          vmerger(a.toString,b.toString,c.toString) name "vmerger"
        case Prim(CPrim("dupl", _, _, _), List(a), List(b, c),_) =>
          repl(a.toString,b.toString,c.toString) name "dupl"
        case Prim(CPrim("vdupl", _, _, _), List(a), List(b, c),_) =>
          vrepl(a.toString,b.toString,c.toString) name "vdupl"
        case Prim(CPrim("writer", _, _, _), List(), List(a),_) =>
          writer(a.toString)
        case Prim(CPrim("reader", _, _, _), List(a), List(),_) =>
          reader(a.toString)
        case Prim(CPrim("noSnk", _, _, _), List(), List(a),_) =>
          noSink(a.toString)
        case Prim(CPrim("noSrc", _, _, _), List(a), List(),_) =>
          noSrc(a.toString)
        // if we use onetooneSimple we need to add support for nodes
        case Prim(CPrim("node",_,_,extra), ins, outs, _) if extra.intersect(Set("vdupl","mrg")) == Set("vdupl","mrg") =>
          mrg2vdupl(ins.map(_.toString).toSet,outs.map(_.toString).toSet) name "vmixed"
        case Prim(CPrim("node",_,_,extra), ins, outs, _) if extra.intersect(Set("dupl","vmrg")) == Set("dupl","vmrg") =>
          vmrg2dupl(ins.map(_.toString).toSet,outs.map(_.toString).toSet) name "vmixed"
        // dupl or mrg (typical mixed node)
        case Prim(CPrim("node",_,_,extra), ins, outs, _) if extra.intersect(Set("dupl","mrg")).nonEmpty =>
          mixed(ins.map(_.toString).toSet,outs.map(_.toString).toSet) name "mixed"
        // vdupl or vmrg (typical vmixed node)
        case Prim(CPrim("node",_,_,extra), ins, outs, _) if extra.intersect(Set("vdupl","vmrg")).nonEmpty =>
          vmixed(ins.map(_.toString).toSet,outs.map(_.toString).toSet) name "vmixed"
        // xor
        case Prim(CPrim("node",_,_,extra), ins, outs, _) if extra contains("xor") =>
          mixedxor(ins.map(_.toString).toSet,outs.map(_.toString).toSet) name "xor"
        // unknown name with type 1->1 -- behave as identity
        case Prim(CPrim(name, _, _, _), List(a), List(b),_) =>
          sync(a.toString,b.toString) name name

        case Prim(p, _, _,_) =>
          throw new GenerationException(s"Unknown ifta automata for primitive $p")
        }
      (IftaAutomata(iFta,Set(iFta),Set(e)),0)
    }

    /** Emtpy IftaAutomata */
    def emptyAutomata: IftaAutomata = IftaAutomata(newifta,Set(),Set())

    /**
      * Automata composition - combining every possible transition,
      * and including transitions that can occur in parallel.
      *
      * @param a1 1st automaton to be composed
      * @param a2 1st automaton to be composed
      * @return composed automata
      */
    def join(a1: IftaAutomata, a2: IftaAutomata): IftaAutomata =
      join(a1,a2,true,20000)

    def join(a1:IftaAutomata,a2:IftaAutomata,hide:Boolean, timeout:Int):IftaAutomata =
      IftaAutomata(a1.ifta.prod(a2.ifta,hide,timeout),a1.nifta++a2.nifta,a1.conns++a2.conns)
  }
}
