Interface Featured Timed Automata
========

This project is a small Scala implementation of the Interface Featured Timed Automata (IFTA), that will include:

 - a small scala DSL
 - composition operator of IFTAs
 - export into UPPAAL Timed Automata with features


## Building primitive IFTAs

To compile and run the tools you can use [sbt](http://www.scala-sbt.org), for example, by executing the command ```sbt console```.
There, you can try the following commands:

```scala
import ifta.DSL._

val myAut = newifta ++ (
    0 --> 1 by "a,b" when "f",
    1 --> 0 by "c" when "g"
    ) startWith 0 when "g"-->"f"
// Create a simple automaton with 2 transitions and initial location "0".
// "f" and "g" are features that must obey the constraint "g -> f", and
// determine which edges exist.

myAut.isValid
// returns "true", meaning that the feature model is non-empty (i.e., satisfiable).

myAut.instance  // or myAut.instance("f") to refine
// finds a concrete product (timed automaton), by selecting a valid product and instantiating the automaton.
```


## Visualising an automata and exporting to [Uppaal](http://uppaal.com)

To debug and analyse automata, two functions are provided to produce an intuitive `dot` graph file and to produce an `xml` file that can be used by Uppaal.

```scala
toDot(myAut)
// outputs the automaton in dot format (for graphviz - https://mdaines.github.io/viz.js)

toUppaal(myAut get "a" pub "b,c","myAut.xml")
// produces an xml file that can be opened with Uppaal model checker, using "a" as an input channel and "b" and "c" as output channels
```

The `dot` output is handy to quickly visualise a connector with an intuitive layout (use, for example online tool [Viz.js](https://mdaines.github.io/viz.js/) to preview the produced graph), while the [Uppaal](http://uppaal.com) output requires some manual layout adjustments but can be used to simulate and to prove (temporal) properties.
Note that the `Uppaal` model was extended to annotations to make `a` an input channel and `b` and `c` output channels, represented as `a?`, `b!`, and `c!` in the Uppaal model.

Screenshots of the `dot` and `Uppaal` outputs follow bellow.
Observe that Uppaal does not support multiple actions per transition - these are rewritten as an interleaving of all combinations, imposing that inputs come before outputs (to reduce the state space).

![alt text](https://cdn.rawgit.com/joseproenca/ifta/master/images/myAut.svg "Automata example in Dot")   ![alt text](https://cdn.rawgit.com/joseproenca/ifta/master/images/myAutUpp.svg "Automata example in Uppaal")

## Time extension

Automata can be extended with clocks, using the same notion of clocks as Uppaal. Hence, states can have clock constraints as invariants, and edges can have clock constraints as guards that make a transition active.

The code below defines a simple coffee machine `cm`  with 2 features `cf` and `mk` (standing for coffee and milk), and with 3 actions: `co` (coffee), `ca` (cappuccino), and `b` (brew).
Time is captured by the clock `c`, that is set to 0 every time the automata evolves to location 1, and is used to specify that the coffee will be brewed some time between 2 and 5 time units after selecting the product.


```scala
val cm = newifta ++ (
    0 --> 1 by "co" when "cf"&&"mk" reset "c",
    0 --> 1 by "ca" when "cf"       reset "c",
    1 --> 0 by "b" cc "c">=2
    ) startWith 0 when "mk"-->"cf" inv(1,"c"<=5) get "co" get "ca" pub "b"
```


## Composing automata

Two automata `a1` and `a2` can be composed in 2 ways:
 - `a1 * a2`: using the product of IFTA, which returns a new IFTA;
 - `a1 || a2`: creating a network of parallel IFTA, without calculating the product.

An example of these compositions follow below, using `sync` to rename ports that are connected (and need to be unified.

```scala
val cm = ... /* as before */

val rtr = newifta ++ (
    0 --> 1 by "i"  when "vi" && ("vo1"||"vo2"),
    1 --> 0 by "o1" when "vi" && "vo1",
    1 --> 0 by "o2" when "vi" && "vo2"
    ) startWith 0 get "i" pub "o1" pub "o2"
// or simply: val rtr = router("i","o1","o2")

val link = List("o1"->"ca","o2"->"co")

val netComp  = (cm  sync link) || (rtr sync link)
val prodComp = (cm  sync link) *  (rtr sync link)
```

The automata of `cm`, `rtr`, and the product `prodComp` are depicted below, respectively.
Observe that both compositions `netComp` and `prodComp` can be exported to Uppaal: the former using a network of automata, and the latter producing a single automaton.

![alt text](https://cdn.rawgit.com/joseproenca/ifta/master/images/cm.svg "Coffee Machine automaton in Dot") ![alt text](https://cdn.rawgit.com/joseproenca/ifta/master/images/router.svg "Router automaton in Dot") ![alt text](https://cdn.rawgit.com/joseproenca/ifta/master/images/cm-router.svg "Product of cm and rtr in Dot")

