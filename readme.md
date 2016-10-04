Interface Featured Timed Automata
========

This project is a small Scala implementation of the Interface Featured Timed Automata (IFTA), that will include:

 - a small scala DSL
 - composition operator of IFTAs
 - export into UPPAAL Timed Automata with features

To compile and run the tools you can use [sbt](http://www.scala-sbt.org), for example, by executing the command ```sbt console```.
There, you can try the following commands:

```scala
import ifta.DSL._

val myAut = ifta +++ (
    0 --> 1 by "a" when "f",
    1 --> 0 by "b" when "g"
    ) startWith 0 when "g"-->"f"
// Create a simple automaton with 2 transitions and inital location "0".
// "f" and "g" are features that must obey the constraint "g -> f", and
// determine which edges exist.

toDot(myAut)
// outputs the automaton in dot format (for graphviz - https://mdaines.github.io/viz.js)

myAut.isValid
// returns "true", meaning that the feature model is non-empty (i.e., satisfiable).

myAut.instance
// finds a concrete product (timed automaton), by selecting a valid product and instantiating the automaton.
```

More complex examples:

```scala
val cm = ifta +++ (
    0 --> 1 by "co" when "cf"&&"mk" reset "c",
    0 --> 1 by "ca" when "cf"       reset "c",
    1 --> 0 by "b" cc "c">=2
    ) startWith 0 when "mk"-->"cf" inv(1,"c"<=5) get "co" get "ca" pub "b"


val router = ifta +++ (
    3 --> 4 by "i"  when "vi" && ("vo1"||"vo2"),
    4 --> 3 by "co" when "vi" && "vo1",
    4 --> 3 by "ca" when "vi" && "vo2"
    ) startWith 3 get "i" pub "co" pub "ca"

val comp = cm * router
toUppal(comp,"file.xml")
```
