Interface Featured Timed Automata
========

This project is a small Scala implementation of the Interface Featured Timed Automata (IFTA), that will include:

 - a small scala DSL
 - composition operator of IFTAs
 - export into UPPAAL Timed Automata with features

Below you will find a quick overview on how to build, visualise, compose, and analyse IFTA using our Scala library.
For more examples, please check our [examples folder](src/main/scala/ifta/examples).

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

To debug and analyse automata, three functions are provided to produce an intuitive `dot` graph file, an `xml` file that can be used by Uppaal, and an interactive `html` file using [Vis.js](http://visjs.org) to visualize an automata and see how it is affected by its possible feature selections.

```scala
toDot(myAut)
// outputs the automaton in dot format (for graphviz - https://mdaines.github.io/viz.js)

toUppaal(myAut get "a" pub "b,c","myAut.xml")
// produces an xml file that can be opened with Uppaal model checker, using "a" as an input channel and "b" and "c" as output channels

// the application IFTA is defined in the examples folder
toVis(application, "application.html")
// produces an html file that can be opened in a browser and see which transitions are enabled in each possible feature selection
```

The `dot` output is handy to quickly visualise a connector with an intuitive layout (use, for example online tool [Viz.js](https://mdaines.github.io/viz.js/) to preview the produced graph). 
The [Uppaal](http://uppaal.com) output requires some manual layout adjustments but can be used to simulate and to prove (temporal) properties.
Note that the `Uppaal` model was extended to annotations to make `a` an input channel and `b` and `c` output channels, represented as `a?`, `b!`, and `c!` in the Uppaal model.
The `html` output is handy to see what possible feature selections are derived from the feature model and how they affect the transitions of the automata.

Screenshots of the `dot`, `Uppaal`, and `Vis.js` outputs follow bellow.
Observe that Uppaal does not support multiple actions per transition - these are rewritten as an interleaving of all combinations, imposing that inputs come before outputs (to reduce the state space).

![alt text](https://cdn.rawgit.com/joseproenca/ifta/master/images/myAut.svg "Automata example in Dot")   ![alt text](https://cdn.rawgit.com/joseproenca/ifta/master/images/myAutUpp.svg "Automata example in Uppaal")
![alt text](https://cdn.rawgit.com/joseproenca/ifta/master/images/app-tovis.gif "Automata example in Vis")
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

// network of 2 automata (communicating over link)
val netComp  = (cm || rtr) sync    link
// flattened of the same 2 automata (communicating over link)
val prodComp = (cm || rtr) product link  // same as (cm sync link)*(rtr sync link)
```

The automata of `cm`, `rtr`, and the product `prodComp` are depicted below, respectively.
Observe that both compositions `netComp` and `prodComp` can be exported to Uppaal: the former using a network of automata, and the latter producing a single automaton.

![alt text](https://cdn.rawgit.com/joseproenca/ifta/master/images/cm.svg "Coffee Machine automaton in Dot") ![alt text](https://cdn.rawgit.com/joseproenca/ifta/master/images/router.svg "Router automaton in Dot") ![alt text](https://cdn.rawgit.com/joseproenca/ifta/master/images/cm-router.svg "Product of cm and rtr in Dot")

In larger examples it is often necessary to visualise the composition of automata. This can be done, for example, as follows.

```scala
con2dot( ((cm name "CM") || (rtr name "Rt")) sync link )
// produces a dot graph depicting how the different IFTA of the network interact 

// seq3net is a NIFTA for a sequencer connector of three outputs. Its definition can be found in the examples folder
con2vis(seq3net, "seq3net.html")
// produces a html file depicting how the different IFTA of the network interact
// and how the feature selections derived from its feature model affect the precense of intefaces
```

![alt text](https://cdn.rawgit.com/joseproenca/ifta/master/images/cm-router-conn.svg "Depicting the connector composing the coffee machine and the router")
![alt text](https://cdn.rawgit.com/joseproenca/ifta/master/images/seq3-con2vis.gif "Depicting the network of a sequencer connector of 3 outputs")