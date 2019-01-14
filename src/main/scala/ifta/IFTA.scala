 package ifta

 import ifta.backend.Show
 import ifta.analyse.Simplify
// import org.scalatest.tools.ReporterConfigurations remove for used in ReoLive

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
                 , in:Set[String], out:Set[String], aps:Map[Int,String], shortname:String="") {

   def goFromBy(loc: Int, bigEdge: Edge): Option[Int] = bigEdge match {
     case Edge(from,cc,as,rs,f,to) =>
       edges.find(e => //e.from==loc &&
                       e.cCons.restrict(clocks) == cc.restrict(clocks) && // may not always work.
                       e.act == as.intersect(act) &&
                       e.cReset == rs.intersect(clocks) /*&&
                       e.fe.restrict(feats) == f.restrict(feats)*/).map(_.to)
//       match {
//         case None => throw new RuntimeException(s"no edge found from $loc by ${Show(bigEdge)} in $this.")
//         case Some(x) => x.to
//       }
   }

   def *(other:IFTA,timeout:Int):IFTA = prod(other,false,timeout)
   def *(other:IFTA):IFTA = prod(other,false, -1)

   def hideProduct(other:IFTA,timeout:Int):IFTA = prod(other,true,timeout)
   def hideProduct(other:IFTA):IFTA = prod(other,true,-1)
   /**
     * Product of 2 IFTAs with timeout
     * @param other
     * @param timeout
     */
    def prod(other:IFTA, hide:Boolean, timeout:Int= -1) = {
      var steps = timeout
      def tick() = {
        if (steps > 1)
          steps -= 1
        else throw new RuntimeException(s"Timeout when composing:\n ${this.toString}\n ${other.toString}")
      }
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
        yield {if (timeout > -1) tick(); e1.join(e2,hide,prod)})            ++
        (for (e1 <- edges; loc <- other.locs; if e1.act.intersect(shared).isEmpty)
          yield {if (timeout > -1) tick(); e1.independent(loc, prod,1)}) ++
        (for (e2 <- other.edges; loc <- locs; if e2.act.intersect(shared).isEmpty)
          yield {if (timeout > -1) tick(); e2.independent(loc, prod,2)})

      val resInv = (for (l1<-loc1; l2<-loc2)
        yield prod(l1, l2) -> CAnd(cInv.withDefaultValue(CTrue)(l1),
          other.cInv.withDefaultValue(CTrue)(l2))).toMap[Int,ClockCons]
      val resFm = (fm && other.fm) &&
        (for (a <- shared) yield fe(a) <-> other.fe(a)).fold(FTrue)(_&&_)
      val resIn = (in ++ other.in) -- shared
      val resOut = (out ++ other.out) -- shared
      val nm =
        if (this.shortname=="" || other.shortname=="") this.shortname+other.shortname
        else  this.shortname+"*"+other.shortname

      var prodLocNames: Map[String,Int] = Map()
      def mkProdLocName(l1:Int,l2:Int):String ={
        var n:String = ""
        (aps.contains(l1), other.aps.contains(l2))  match {
          case (true, true) =>
            validLocName(aps.get(l1).get+"_"+other.aps.get(l2).get)
          case (true,false) => validLocName(aps.get(l1).get)
          case (false,true) => validLocName(other.aps.get(l2).get)
          case (_,_) => ""
        }
      }
      def validLocName(n:String):String = {
        var index:Int = 0
        var res:String = n
        if (prodLocNames.contains(n)) {
          index = nameOffset(n,prodLocNames.get(n).get)
          res += index
          prodLocNames -= n
          prodLocNames += res -> 1
        }
        prodLocNames += n -> (index+1)
        res
      }

      def nameOffset(n:String,index:Int):Int =
        if (!prodLocNames.contains(n+index)) index
        else nameOffset(n,index+1)


      var resAut = Simplify.removeUnreach(IFTA(resLocs,resInit,resAct,resCl,resFeat,resEdges,resInv,resFm,resIn,resOut,Map(),nm))

      resAut ap (for (l1<-loc1;l2<-loc2; if ((resAut.locs contains prod(l1,l2)) && (aps.contains(l1) || other.aps.contains(l2))))
        yield prod(l1,l2) -> mkProdLocName(l1,l2))
    }

//   /**
//     * Product of 2 IFTAs synchronising shared ports
//     *
//     * @param other
//     * @return
//     */
//   def *(other:IFTA): IFTA = {
//     // index of loc1 and loc2 will be used to new locations
//     val loc1 = locs.toList
//     val loc2 = other.locs.toList
//     val base = loc1.length
//     // l1 x l2 = index(l1) + index(l2)*base
//     def prod(l1:Int,l2:Int) =
//       loc1.indexOf(l1)+loc2.indexOf(l2)*base
//
//     val shared = act intersect other.act
//
//     val resLocs = (0 until (loc1.length * loc2.length)).toSet
//     val resInit = prod(init,other.init)
////       for (i <- init; j <- other.init) yield prod(i,j)
//
//     val resAct = (act ++ other.act) -- shared
//     val resCl  = clocks ++ other.clocks
//     val resFeat = feats ++ other.feats
//
//     val resEdges = (for (e1 <- edges; e2 <- other.edges; if e1.compat(e2,shared))
//                      yield e1.join(e2,prod))            ++
//                    (for (e1 <- edges; loc <- other.locs; if e1.act.intersect(shared).isEmpty)
//                      yield e1.independent(loc, prod,1)) ++
//                    (for (e2 <- other.edges; loc <- locs; if e2.act.intersect(shared).isEmpty)
//                      yield e2.independent(loc, prod,2))
//
//     val resInv = (for (l1<-loc1; l2<-loc2)
//         yield prod(l1, l2) -> CAnd(cInv.withDefaultValue(CTrue)(l1),
//                               other.cInv.withDefaultValue(CTrue)(l2))).toMap[Int,ClockCons]
//     val resFm = (fm && other.fm) &&
//       (for (a <- shared) yield fe(a) <-> other.fe(a)).fold(FTrue)(_&&_)
//     val resIn = (in ++ other.in) -- shared
//     val resOut = (out ++ other.out) -- shared
//     val nm =
//       if (this.shortname=="" || other.shortname=="") this.shortname+other.shortname
//       else  this.shortname+"*"+other.shortname
//
//     var prodLocNames: Map[String,Int] = Map()
//     def mkProdLocName(l1:Int,l2:Int):String ={
//       var n:String = ""
//       (aps.contains(l1), other.aps.contains(l2))  match {
//         case (true, true) =>
//           validLocName(aps.get(l1).get+"_"+other.aps.get(l2).get)
//         case (true,false) => validLocName(aps.get(l1).get)
//         case (false,true) => validLocName(other.aps.get(l2).get)
//         case (_,_) => ""
//       }
//     }
//     def validLocName(n:String):String = {
//       var index:Int = 0
//       var res:String = n
//       if (prodLocNames.contains(n)) {
//         index = nameOffset(n,prodLocNames.get(n).get)
//         res += index
//         prodLocNames -= n
//         prodLocNames += res -> 1
//       }
//       prodLocNames += n -> (index+1)
//       res
//     }
//
//     def nameOffset(n:String,index:Int):Int =
//       if (!prodLocNames.contains(n+index)) index
//       else nameOffset(n,index+1)
//
//
//     var resAut = Simplify.removeUnreach(IFTA(resLocs,resInit,resAct,resCl,resFeat,resEdges,resInv,resFm,resIn,resOut,Map(),nm))
//
//     resAut ap (for (l1<-loc1;l2<-loc2; if ((resAut.locs contains prod(l1,l2)) && (aps.contains(l1) || other.aps.contains(l2))))
//       yield prod(l1,l2) -> mkProdLocName(l1,l2))
//   }

   // todo - fix!
   def *(other:IFTA,pair:(String,String)): IFTA =
    this.sync(pair) * (other.sync(pair))

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
           , in,out,aps) // in out could be filtered to only valid ports?
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
   def fe(port:String) :FExp =
     (for ( e <- edges; if e.act contains port) yield e.fe).fold(FNot(FTrue))(_||_)
   //  (for ( e <- edges; if e.act contains port) yield e.fe).reduce(_||_) // fails with empty list
   //  edges.filter(_.act contains port).map(_.act).fold(FTrue)(_||_) // alternative

   /**
     *
     * @return set of products modeled by the feature model
     */
   def products = this.fm.products(this.feats)

   /**
     * Inferred feature model:
     * The logic OR of the fe of all actions, and the posible case were all feats are not present
     * This is, the fm at least should satisfy the fe of all transitions present, or all of them missing
     * @return
     */
   def inferredFm: FExp =
     this.act.map(a => fe(a)).fold(FNot(FTrue))(_ || _) || this.feats.map(f => FNot(Feat(f))).fold(FTrue)(_ && _)

   /**
     *
     * @return set of products modeled by the inferred feature model
     */
   def inferredProducts = this.inferredFm.products(this.feats)

   /**
     * Synchronises 2 actions: replaces every action a1 and a2 by a1|a2.
     *
     * @param pair (a1,a2) of actions to be synchronised
     * @return updated IFTA
     */
   private def syncOnce(pair:(String,String)): IFTA =
     IFTA(locs,init,act.map(merge(_,pair._1,pair._2)),clocks,feats
         ,edges.map(e => e by e.act.map(merge(_, pair._1, pair._2))),cInv
         ,syncfe(fm,pair._1,pair._2),in.map(merge(_,pair._1,pair._2)),out.map(merge(_,pair._1,pair._2)),aps,shortname=this.shortname)

   // TODO: control both actions are part of the IFTA
   def sync(pair:(String,String)*): IFTA = sync(pair.toSeq)
   def sync(pair:Iterable[(String,String)]): IFTA =
     if (pair.isEmpty) this
     else this.syncOnce(pair.head).sync(pair.tail)

   private def syncfe(fm: FExp, a1:String,a2:String):FExp =
     if ((act contains a1) && (act contains a2))
       fm && (fe(a1) <-> fe(a2))
     else fm

   /**
     * TODO: to be changed; merge should receive the set of actions of the edge
     * and checked that both are present in the edge, otherwise, it should remove
     * the edge.
     */
   private def merge(a:String,a1:String,a2:String): String =
     if (a == a1 || a == a2) a1+"_"+a2 else a

   /**
     * Merges the FM of this and other IFTA
     * @param other
     * @return
     */
   def mergeFM(other:IFTA):IFTA ={
     val shared = act intersect other.act
     val resFM: FExp = (fm && other.fm) &&
       (for (a <- shared) yield fe(a) <-> other.fe(a)).fold(FTrue)(_&&_)
     IFTA(Set(),0,act++other.act,Set(),feats++other.feats,edges++other.edges,Map(),resFM,Set(),Set(),Map())
   }

   def filter(loc:Int) =
     IFTA(locs,init,act,clocks,feats,edges.filter(_.from == loc),cInv,fm,in,out,aps,shortname)

   // constructors
   private def link(e: Edge): IFTA =
     IFTA(locs+e.from+e.to,init,act++e.act,clocks++e.cCons.clocks++e.cReset,feats++e.fe.feats,edges+e,cInv,fm,in,out,aps,shortname)
   def ++(e:Edge*) = {
     var res = this
     for (ed <- e) res = res link ed
     res
   }
   def when(f:FExp): IFTA =
     IFTA(locs,init,act,clocks,feats++f.feats,edges,cInv,fm && f,in,out,aps,shortname)
   def get(p:String): IFTA =
     IFTA(locs,init,act++p.split(","),clocks,feats,edges,cInv,fm,in++p.split(","),out,aps,shortname)
   def pub(p:String): IFTA =
     IFTA(locs,init,act++p.split(","),clocks,feats,edges,cInv,fm,in,out++p.split(","),aps,shortname)
   def startWith(i:Int): IFTA =
     IFTA(locs,init+i,act,clocks,feats,edges,cInv,fm,in,out,aps,shortname)
   def inv(l:Int,cc:ClockCons): IFTA =
     IFTA(locs+l,init,act,clocks++cc.clocks,feats,edges,cInv+(l->cc),fm,in,out,aps,shortname)
   def name(n:String): IFTA =
     IFTA(locs,init,act,clocks,feats,edges,cInv,fm,in,out,aps,shortname=n)
   def ap(l:Int,n:String):IFTA = {
//     if (aps.contains(l))
//       throw new RuntimeException("Location name already exists")
     IFTA(locs, init, act, clocks, feats, edges, cInv, fm, in, out, aps + (l -> n), shortname)
   }
   def ap(pair:(Int,String)*):IFTA =
     IFTA(locs,init,act,clocks,feats,edges,cInv,fm,in,out,aps++pair.toMap,shortname)
   def ap(pair:Iterable[(Int,String)]):IFTA =
     IFTA(locs,init,act,clocks,feats,edges,cInv,fm,in,out,aps++pair.toMap[Int,String],shortname)

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
   def join(other:Edge,hide:Boolean, prod:(Int,Int)=>Int): Edge =
     Edge(prod(from,other.from), CAnd(cCons, other.cCons)
       ,if (hide) act++other.act -- act.intersect(other.act) else act++other.act
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
   def reset(c:String) = Edge(from,cCons,act,cReset++c.split(",").toSet,fe,to)
   def reset(c:Iterable[String]) = Edge(from,cCons,act,cReset++c.toSet,fe,to)
   def by(a:String) = Edge(from,cCons,a.split(",").toSet,cReset,fe,to)
   def by(as:Iterable[String]) = Edge(from,cCons,as.toSet,cReset,fe,to)
   def cc(c:ClockCons) = Edge(from,CAnd(cCons,c),act,cReset,fe,to)
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
//   def sync(pair:(String,String)): NIFTA =
//    NIFTA(iFTAs.map(_.sync(pair)))

   def sync(pair:(String,String)*): NIFTA = sync(pair.toSeq)

   //todo: fix sync for networks, find correct fEPort and fEPort(pair._1) <-> fEPort(pair._2)
   def sync(pair:Iterable[(String,String)]): NIFTA =
     if (pair.isEmpty) this
     else NIFTA(iFTAs.map(_.sync(pair.head))).sync(pair.tail)


   /**
     * Flattens a network of IFTAS into a single (product) IFTA, after linking ports that need to be combined.
     * In practice, it uses "sync" followed by a fold of i1*i2.
     * @param pair Nodes
     * @return
     */

   def product(pair:(String,String)*): IFTA = product(-1,pair.toSeq)
   def product(timeout:Int, pair:(String,String)*): IFTA = product(timeout,pair.toSeq)
   def hideProduct(timeout:Int, pair:(String,String)*): IFTA = product(true,timeout,pair.toSeq)
   def hideProduct(pair:(String,String)*): IFTA = product(true,-1,pair.toSeq)
   def product(pair:Iterable[(String,String)]): IFTA = product(false,-1, pair)
   def hideProduct(timeout:Int = -1, pair:Iterable[(String,String)]): IFTA = product(true,timeout, pair)
   def product(timeout:Int = -1, pair:Iterable[(String,String)]): IFTA = product(false,timeout, pair)
   private def product(hiding:Boolean, timeout:Int, pair:Iterable[(String,String)]): IFTA = {
     val synched = this.sync(pair)
     synched.iFTAs.headOption match {
       case Some(x) => if (hiding) synched.iFTAs.tail.fold(x)(_.hideProduct(_,timeout)) else synched.iFTAs.tail.fold(x)(_*(_,timeout))
       case None => DSL.newifta
     }
   }


   /**
     * Flattens a network of IFTAs into a single IFTA using `product`.
     * @timeout -1 -> no timeout;  n>-1 number of steps to compose all automata in the net
     * @return
     */
   def hideFlatten(timeout:Int = -1) = hideProduct(timeout)
   def flatten(timeout:Int): IFTA = product(timeout)
   def flatten:IFTA = product(-1)
   /**
     * Flattens a network of IFTAs using a step-by-step approach.
     * @return single IFTA restulting from composing all IFTAs in the network
     */
   // TODO: unfinished - probably start by replacing int by something else for locations of IFTAs
   def flatten2: Set[IFTA] = {

     val auts = iFTAs.toList // to fix the order

     var done = Set[IFTA]()
     var toGo = Map[IFTA,List[Int]](NIFTA(iFTAs.map(aut => aut.filter(aut.init))).flatten -> auts.map(_.init))
     // composition of aut restr. to the initial state, and initial state as a list.

     while ((toGo -- done).nonEmpty) {
       val aut = (toGo -- done).head
       done += aut._1

       for (e <- aut._1.edges) {
         // get the state from "state aut._2" for the "automata aut._1"
         val nMbState = auts.zip(aut._2).map(pair=>pair._1.goFromBy(pair._2,e))
         val nState:List[Int] = nMbState.zip(aut._2).map(pair => pair._1.getOrElse(pair._2))
         val nAut:IFTA = NIFTA(auts.zip(nState).map(a => a._1.filter(a._2)).toSet).flatten
         toGo += nAut -> nState
       }
     }
     done
   }

//   {  val parts = iFTAs.toList
//     val init = parts.map(_.init)
//     flattenAux(parts,Set(init),Set(),0)._1
//   }
//   private def flattenAux(all:List[IFTA],next:Set[List[Int]],done:Set[List[Int]],seed:Int) : (IFTA,Int) =
//     if (next.isEmpty)
//       (ifta.DSL.newifta,seed)
//     else {
//       var next2 = Nil: List[List[Int]]
//       for (ns <- next; (a, st) <- all.zip(ns)) {
//         // for all aut a in (next) state st
//         // - check if a is dependent with any other aut in `all`
//
//
//       }
//
//       (ifta.DSL.newifta, seed)
//     }


   /**
     * Adds conjunctively a feature expression to a network of IFTA
     * @param fe Feature expression to be added
     * @return new constrained NIFTA
     */
   def when(fe:FExp): NIFTA =
     this || (DSL.newifta when fe)


   def fm =
       Simplify(iFTAs.fold(IFTA(Set(),0,Set(),Set(),Set(),Set(),Map(),FTrue,Set(),Set(),Map()))(_ mergeFM _).fm)


   // constructors
   def ||(i:IFTA):NIFTA = NIFTA(iFTAs+i)
   def ||(n:NIFTA):NIFTA = NIFTA(iFTAs++n.iFTAs)

   override def toString = iFTAs.map(Show.apply).mkString(" ||\n")
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