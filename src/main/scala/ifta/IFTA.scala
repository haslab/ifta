 package ifta

 /**
   * Created by jose on 30/09/16.
   */

 /**
   * Represents an Interface Featured Timed Automata (IFTA)
   * @param locs locations (states of the automata)
   * @param init initial locations
   * @param act actions
   * @param clocks clocks
   * @param feats features
   * @param edges edges
   * @param inv invariants of locations
   * @param fm feature model
   * @param vars variables (of feature expressions)
   * @param in input ports
   * @param out output ports
   */
 case class IFTA(locs:Set[Int],init:Set[Int],act:Set[String],clocks:Set[String]
                 ,feats:Set[String],edges:Set[Edge],inv:Map[Int,FExp],fm:FExp
                 ,vars:Set[String],in:Set[String],out:Set[String]) {

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
     val resInit = for (i <- init; j <- other.init)
                      yield prod(i,j)

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
         yield prod(l1, l2) -> FAnd(inv.withDefaultValue(FTrue)(l1),
                              other.inv.withDefaultValue(FTrue)(l2))).toMap[Int,FExp]
     val resFm = FAnd(fm,other.fm)
     val resVars = vars ++ other.vars
     val resIn = (in ++ other.in) -- shared
     val resOut = (out ++ other.out) -- shared

     IFTA(resLocs,resInit,resAct,resCl,resFeat,resEdges,resInv,resFm,resVars,resIn,resOut)
   }

 }

 /**
   * Represents an edge of an IFTA
   * @param from source location
   * @param cc clock constraint
   * @param act actions
   * @param reset clocks to be reset
   * @param fe feature expression (guard)
   * @param to destination location
   */
 case class Edge(from:Int,cc:ClockCons,act:Set[String]
                 ,reset:Set[String],fe:FExp,to:Int) {
   /**
     * Checks if 2 edges are compatible (have the same shared actions)
     * @param other edge to compare against
     * @param shared edges that must match
     * @return
     */
   def compat(other:Edge,shared:Set[String]) =
     (act intersect shared) == (other.act intersect shared)

   /**
     * Combines 2 compatible edges (pointwise union/conjunction)
     * @param other edge to combine with
     * @param prod function to combine location numbers
     * @return
     */
   def join(other:Edge,prod:(Int,Int)=>Int): Edge =
     Edge(prod(from,other.from), CAnd(cc,other.cc), act++other.act
         ,reset++other.reset,FAnd(fe,other.fe),prod(to,other.to))
   
   /**
    * Creates an edge taken independently during composition 
    * @param loc location of the other automata for composed transition
    * @param prod function to combine location numbers
    * @param pos whether the edge is from automata A1 or A2 (in A1xA2)
    */
   def independent(loc:Int,prod:(Int,Int) => Int, pos:Int): Edge = 
     if (pos == 1) Edge(prod(from,loc), cc, act,reset,fe, prod(to,loc)) 
     else          Edge(prod(loc,from), cc, act,reset,fe, prod(loc,to))
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