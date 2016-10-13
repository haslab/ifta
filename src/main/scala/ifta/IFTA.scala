 package ifta

 import ifta.backend.Show
 import org.scalatest.tools.ReporterConfigurations

 /**
   * Created by jose on 30/09/16.
   */

 /**
   * Represents an Interface Featured Timed Automata (IFTA)
   *
   * @param locs locations (states of the automata)
   * @param init initial locations
   * @param act actions
   * @param clocks clocks
   * @param feats features
   * @param edges edges
   * @param cInv invariants of locations
   * @param fm feature model
   * @param in input ports
   * @param out output ports
   */
 case class IFTA(locs:Set[Int], init:Int, act:Set[String], clocks:Set[String]
                 , feats:Set[String], edges:Set[Edge], cInv:Map[Int,ClockCons], fm:FExp
                 , in:Set[String], out:Set[String]) {

   /**
     * Product of 2 IFTAs synchronising shared ports
     *
     * @param other
     * @return
     */
   def *(other:IFTA): IFTA = {
     // index of loc1 and loc2 will be used to new locations
     val loc1 = locs.toList
     val loc2 = other.locs.toList
     val base = loc1.length
     // l1 x l2 = index(l1) + index(l2)*base
     def prod(l1:Int,l2:Int) =
       loc1.indexOf(l1)+loc2.indexOf(l2)*base

     val shared = act intersect other.act

     val resLocs = (0 until (loc1.length * loc2.length)).toSet
     val resInit = prod(init,other.init)
//       for (i <- init; j <- other.init) yield prod(i,j)

     val resAct = (act ++ other.act) -- shared
     val resCl  = clocks ++ other.clocks
     val resFeat = feats ++ other.feats

     val resEdges = (for (e1 <- edges; e2 <- other.edges; if e1.compat(e2,shared))
                      yield e1.join(e2,prod))            ++
                    (for (e1 <- edges; loc <- other.locs; if e1.act.intersect(shared).isEmpty)
                      yield e1.independent(loc, prod,1)) ++
                    (for (e2 <- other.edges; loc <- locs; if e2.act.intersect(shared).isEmpty)
                      yield e2.independent(loc, prod,2))

     val resInv = (for (l1<-loc1; l2<-loc2)
         yield prod(l1, l2) -> CAnd(cInv.withDefaultValue(CTrue)(l1),
                               other.cInv.withDefaultValue(CTrue)(l2))).toMap[Int,ClockCons]
     val resFm = (fm && other.fm) &&
       (for (a <- shared) yield featExpPorts(a) <-> other.featExpPorts(a)).fold(FTrue)(_&&_)
     val resIn = (in ++ other.in) -- shared
     val resOut = (out ++ other.out) -- shared

     IFTA(resLocs,resInit,resAct,resCl,resFeat,resEdges,resInv,resFm,resIn,resOut)
   }

   lazy val interface:Set[String] = in ++ out

   /**
     * Checks if the IFTA is valid, i.e., if its feature model has solutions
     */
   lazy val isValid:Boolean =
      analyse.Solver(fm).isDefined
   /**
     * Finds a solution for its feature model and uses it to project into a timed automata,
     * i.e., a IFTA without the feature model
     */
   lazy val instance:IFTA = {
     analyse.Solver(fm) match {
       case Some(sol) =>
         IFTA(locs,init,act,clocks
           ,(for ((s,b)<-sol; if b && (feats contains s)) yield s).toSet
           , for (e <- edges; if e.fe.check(sol)) yield e when FTrue
           , cInv,FTrue
           , in,out) // in out could be filtered to only valid ports?
       case None => DSL.newifta
     }
   }

   /**
     * Finds a concrete instance of the IFTA given an extra restriction
     *
     * @param f New restriction to the IFTA (e.g., "f" must be present)
     * @return concrete IFTA
     */
   def instance(f:FExp):IFTA =
     (this when (this.fm && f)).instance

   /**
     * Feature expression of a port, defined as
     * OR of all fexp associated to edges (_,_,{_,port,_},_,_)
 *
     * @param port
     * @return
     */
   private def fEPort(port:String) :FExp =
     (for ( e <- edges; if e.act contains port) yield e.fe).fold(FNot(FTrue))(_||_)
   //  (for ( e <- edges; if e.act contains port) yield e.fe).reduce(_||_) // fails with empty list
   //  edges.filter(_.act contains port).map(_.act).fold(FTrue)(_||_) // alternative

   /**
     * Feature expression of each port
     * for now uses all actions, but it should only be define for ports
     */
   val featExpPorts:Map[String,FExp] =
     (act map {a => a -> fEPort(a) }).toMap

   /**
     * Synchronises 2 actions: replaces every action a1 and a2 by a1|a2.
     *
     * @param pair (a1,a2) of actions to be synchronised
     * @return updated IFTA
     */
   def sync(pair:(String,String)): IFTA =
     IFTA(locs,init,act.map(merge(_,pair._1,pair._2)),clocks,feats
         ,edges.map(e => e by e.act.map(merge(_, pair._1, pair._2))),cInv
         ,fm,in.map(merge(_,pair._1,pair._2)),out.map(merge(_,pair._1,pair._2)))

   def sync(pair:(String,String)*): IFTA = syncA(pair.toSeq)
   private def syncA(pair:Seq[(String,String)]): IFTA =
     if (pair.isEmpty) this
     else this.sync(pair.head).syncA(pair.tail)

   private def merge(a:String,a1:String,a2:String): String =
     if (a == a1 || a == a2) a1+"_"+a2 else a

   // constructors
   def ++(e: Edge): IFTA =
     IFTA(locs+e.from+e.to,init,act++e.act,clocks++e.cCons.clocks,feats++e.fe.feats,edges+e,cInv,fm,in,out)
   def +++(e:Edge*) = {
     var res = this
     for (ed <- e) res = res++ed
     res
   }
   def when(f:FExp): IFTA =
     IFTA(locs,init,act,clocks,feats,edges,cInv,fm && f,in,out)
   def get(p:String): IFTA =
     IFTA(locs,init,act,clocks,feats,edges,cInv,fm,in+p,out)
   def pub(p:String): IFTA =
     IFTA(locs,init,act,clocks,feats,edges,cInv,fm,in,out+p)
   def startWith(i:Int): IFTA =
     IFTA(locs,init+i,act,clocks,feats,edges,cInv,fm,in,out)
   def inv(l:Int,cc:ClockCons): IFTA =
     IFTA(locs,init,act,clocks,feats,edges,cInv+(l->cc),fm,in,out)

   // build a NIFTA
   def ||(i:IFTA):NIFTA = NIFTA(Set(this,i))
   def ||(n:NIFTA):NIFTA = NIFTA(n.iFTAs+this)

   override def toString = Show(this)
 }

 /**
   * Represents an edge of an IFTA
 *
   * @param from source location
   * @param cCons clock constraint
   * @param act actions
   * @param cReset clocks to be reset
   * @param fe feature expression (guard)
   * @param to destination location
   */
 case class Edge(from:Int, cCons: ClockCons, act:Set[String]
                 ,cReset:Set[String], fe:FExp, to:Int) {
   /**
     * Checks if 2 edges are compatible (have the same shared actions)
 *
     * @param other edge to compare against
     * @param shared edges that must match
     * @return
     */
   def compat(other:Edge,shared:Set[String]) =
     (act intersect shared) == (other.act intersect shared)

   /**
     * Combines 2 compatible edges (pointwise union/conjunction)
 *
     * @param other edge to combine with
     * @param prod function to combine location numbers
     * @return
     */
   def join(other:Edge,prod:(Int,Int)=>Int): Edge =
     Edge(prod(from,other.from), CAnd(cCons, other.cCons), act++other.act
         ,cReset++other.cReset,FAnd(fe,other.fe),prod(to,other.to))

   /**
    * Creates an edge taken independently during composition
 *
    * @param loc location of the other automata for composed transition
    * @param prod function to combine location numbers
    * @param pos whether the edge is from automata A1 or A2 (in A1xA2)
    */
   def independent(loc:Int,prod:(Int,Int) => Int, pos:Int): Edge =
     if (pos == 1) Edge(prod(from,loc), cCons, act,cReset,fe, prod(to,loc))
     else          Edge(prod(loc,from), cCons, act,cReset,fe, prod(loc,to))

   // constructors (replace parameters)
   def reset(c:String) = Edge(from,cCons,act,Set(c),fe,to)
   def reset(c:Iterable[String]) = Edge(from,cCons,act,c.toSet,fe,to)
   def by(a:String) = Edge(from,cCons,Set(a),cReset,fe,to)
   def by(as:Iterable[String]) = Edge(from,cCons,as.toSet,cReset,fe,to)
   def cc(c:ClockCons) = Edge(from,c,act,cReset,fe,to)
   def when(f:FExp) = Edge(from,cCons,act,cReset,f,to)
 }

 /**
   * Represents a network of IFTA
 *
   * @param iFTAs
   */
 case class NIFTA(iFTAs:Set[IFTA]) {

   lazy val interfaces:Set[String] =
     (for (iFTA <- iFTAs) yield iFTA.interface).flatten

   /**
     * Synchronises 2 actions: replaces every action a1 and a2 by a1|a2.
     *
     * @param pair (a1,a2) of actions to be synchronised
     * @return updated IFTA
     */
   def sync(pair:(String,String)): NIFTA =
    NIFTA(iFTAs.map(_.sync(pair)))

   def sync(pair:(String,String)*): NIFTA = syncA(pair.toSeq)
   private def syncA(pair:Seq[(String,String)]): NIFTA =
     if (pair.isEmpty) this
     else this.sync(pair.head).syncA(pair.tail)


   // constructors
   def ||(i:IFTA):NIFTA = NIFTA(iFTAs+i)
   def ||(n:NIFTA):NIFTA = NIFTA(iFTAs++n.iFTAs)

   override def toString = iFTAs.map(Show.apply).mkString(" ||\n")

//   def <<>>(i:IFTA):NIFTA = NIFTA(iFTAs++Set(i))
//   def <<>>(i:IFTA*):NIFTA = {
//     var res = this
//     for (iFTA <- i) res = res<<>>iFTA
//     res
//   }
 }


 /*
 import ifta._
 val i = IFTA(Set(0,1,2,3),Set(0),Set("a","b"),Set(),Set(),
          Set(Edge(0,CTrue,Set("a"),Set(),FTrue,1)),
          Map(), FTrue, Set(), Set("a"),Set("b"))
 val j = IFTA(Set(2,3,4,5),Set(2),Set("b","c"),Set(),Set(),
          Set(Edge(2,CTrue,Set("c"),Set(),FTrue,3)),
          Map(), FTrue, Set(), Set("a"),Set("b"))
  */

 /*
//Example from paper Coffee machine (cm) and Router.
val cm = IFTA(
    Set(0,1),
    Set(0),
    Set("c","ca","b"),
    Set(),
    Set(),
    Set(
        Edge(0,CTrue,Set("c"),Set(),FTrue,1),
        Edge(0,CTrue,Set("ca"),Set(),FTrue,1),
        Edge(1,CTrue,Set("b"),Set(),FTrue,0)), 
    Map(),
    FTrue,
    Set(),
    Set("c","ca"),
    Set("b"))

val router = 
IFTA(
    Set(3,4),
    Set(3),
    Set("c","ca","i"),
    Set(),
    Set(),
    Set(
        Edge(3,CTrue,Set("i"),Set(),FTrue,4),
        Edge(4,CTrue,Set("c"),Set(),FTrue,3),
        Edge(4,CTrue,Set("ca"),Set(),FTrue,3)),
    Map(),
    FTrue,
    Set(),
    Set("i"),
    Set("c","ca"))
*/