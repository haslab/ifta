# Reo connectors as Interface Featured Timed Automata (IFTA)

This folder contains IFTA constructors for some common Reo connectors, including: `sync`, `syncDrain`, `asyncDrain`, `fifo1`, `fifo1full`, `router`, `replicator`, `merger`, and `join`.
 
These constructors can be used to build more complex orchestration mechanism by means of IFTA composition or synchronization.

They can be used by importing the corresponding class:
```scala
import ifta.reo.Connectors._
```
<!--## Semantics of Reo Connectors-->

## Reo

Reo is a channel-based exogenous coordination language where complex coordinators, called connectors, are compositionally built out of simpler ones, called channels. Exogenous coordination facilitates anonymous communication of components. Each connector has a set of input and output ports, and a formal semantics of how data flows from the inputs to the outputs. For our purpose, we abstract from the notion of data and rather concentrate on how execution of actions associated to input ports enable execution of actions associated to output ports.

## Using IFTA Connectors

Modeling Reo connectors as IFTA can enable them with variable behavior based on the presence of ports connected, through synchronization, to their ports. 
Initially, each connector exhibits a **fixed** behavior, i.e. it behaves as its corresponding Reo connector where either all ports are present or all ports are missing. 
We provide two types of operations to manipulate connectors, which can modify their **variability** and **behavior**. 

### Enabling Connectors with Variability
We use the operation `relax` over a connector in order to allow its ports to be variable, this is, each port can or not be present.
We can impose additional variability restrictions by adding feature restrictions to its feature model using the operation `when` followed by a feature expression. 

### Restricting Connectors Behavior
In addition to modifying the variability of a connector we can affect the way it behaves. 
We can say that an specific action, or set of atomic actions, can execute only if some ports are **not present**, or conversely, only if they **are present**.
We use the operation `excludes` and `requires`, respectively, for this purpose.   

Informally, given any connector `conn` with actions `a`,`b` and `c` :
```scala
// con must be relaxed before affecting its behavior
conn.relax excludes "a" -> "b,c" // a transition with an unique action `a` is exclusive with respect to `b` and `c`,i.e.:
// can execute if a is present and if b and c are not present.
conn.relax excludes "b,c" // all transition in conn are exclusive with respect to `b` and `c`, i.e.:
// can execute if the transitions' actions are present and if 
// b(c) is not present in case b(c) is not part of the transitions' actions.

conn.relax requires "a,b" -> "c" // a transition with atomic actions `a` and `b` requires the presence of `c` to be enabled.
conn.relax requires "c" // all transitions in conn require the presence of `c`.
```

We further explain the use of these operation by explaining the semantics of some connectors in terms of their standard and variable behavior. 
In addition we show how they can be constructed, composed and visualized using the provided DSL. 

### Sync
The `sync` connector consists of an input `i` and an output `o` port. In its standard behavior, it synchronizes its input port `i` with its output port `o`, meaning they must execute atomically. 
This behavior can be `relaxed`, enabling the connector to either behave in its standard way, or to receive inputs without any constraints or further action.
Since the latter can be too permissive, we can restricted it to behave in this way only if the output port is not present.
We can specify this as follows:

```scala
// standard behavior: i and o execute atomically
val mysync = sync("i","o")
// variable behavior: either i and o execute atomically, or i executes independently
val mysyncVar = mysiync.relax
// variable restricted behavior: 
// i and o execute atomically if both present, or i can execute indepndently if o is not present
val mysyncVarRes = mysyncVar excludes "i" -> "o"
```

We can visualized each of this connectors in Graphviz using `toDot(mysync)`,`toDot(mysyncVar)`, and `toDot(mysyncVarRes)`:

![alt text](https://cdn.rawgit.com/joseproenca/ifta/master/src/main/scala/ifta/reo/images/reo-sync.svg "IFTA view of a Sync with standard behavior")
![alt text](https://cdn.rawgit.com/joseproenca/ifta/master/src/main/scala/ifta/reo/images/reo-sync-relax.svg "IFTA view of a Sync with variable behavior")
![alt text](https://cdn.rawgit.com/joseproenca/ifta/master/src/main/scala/ifta/reo/images/reo-sync-relax-excludes.svg "IFTA view of a Sync with variable restricted behavior")

### SyncDrain
The `syncDrain` connector consists of two inputs `i1` and `i2`. In its standard behavior, both inputs must execute atomically. 
When relaxed, each input can be present or not. It can be restricted to behave as and `asyncDrain` connector if and only if an input is present and the other not.
We can specify this as follows:

```scala
// standard behavior: i1 and i2 execute atomically
val mysdrain = sdrain("i1","i2")
// variable behvarios: i1 and i2 can execute atomically, or independently.
val mysdrainVar = mysdrain.relax
// variable behvarios: i1 and i2 can execute atomically, or independently if the other one is missing.
val mysdrainVarRes = mysdrainVar excludes "i1,i2"
// excludes "i1,i2" is equivalent in this case to: excludes "i1" -> "i2"  excludes "i2" -> "i1"
```

We can visualized each of this connectors in Graphviz using `toDot(mysdrain)`, `toDot(mysdrainVar)`, and `toDot(mysdrainVarRes)`:

![alt text](https://cdn.rawgit.com/joseproenca/ifta/master/src/main/scala/ifta/reo/images/reo-sdrain.svg "IFTA view of a Syncdrain with standard behavior")
![alt text](https://cdn.rawgit.com/joseproenca/ifta/master/src/main/scala/ifta/reo/images/reo-sdrain-relax.svg "IFTA view of a Syncdrain with variable behavior")
![alt text](https://cdn.rawgit.com/joseproenca/ifta/master/src/main/scala/ifta/reo/images/reo-sdrain-relax-excludes.svg "IFTA view of a Syncdrain with variable restricted behavior")

<!--### AsyncDrain-->

<!--The `asyncDrain` connector consists of two inputs `i1` and `i2`. Receives inputs in either of its ports without further action. -->

<!--It can be specified as follows-->
<!--```scala-->
<!--val myasdrain = asdrain("i1","i2")-->
<!--```-->
<!--and visualized in Graphviz using `toDot(myasdrain)`:-->

<!--![alt text](https://cdn.rawgit.com/joseproenca/ifta/master/src/main/scala/ifta/reo/images/asdrain.svg "AsyncDrain connector as IFTA")-->


### Replicator 

The `replicator` connector consists of an input `i` and two or more outputs `o1,o2,...`. 
In its standard behavior it synchronizes all inputs with the output port, i.e., they all must execute atomically.
When relaxed, each port can be present or not and they can execute atommically or in any possible combination. 
It can be restricted to allow a combination of actions if an only if all others actions are not present.
 <!--e.g.: if any of its outputs is missing, only the outputs that are present are synchonized with the input, and if the input is missing, it behaves as a `syndrain` synchronizing the execution of all the inputs that are present. -->
We can specify this as follows:

```scala
// replicator with 2 outputs with standard behavior: i, o1, and o2 execute atomically
val repl2 = repl("i","o1","o2")
// replicator with 2 outputs with variable behavior:
// i, o1, and o2 execute atomically all together or in any possible combination
val repl2var = repl2.relax
// replicator with 2 outputs with variable restricted behavior:
// i, o1, and o2 execute atomically all together or in any possible combination if and only if the other actions are not present
// possible combinations: i iff "o1,o2" not present; i,o1 iff "o2" not present; i,o2 iff "o1" not present
val repl2varRes = repl2var excludes "o1,o2"

// replicator with 3 outputs with standard behavior: i, o1, o2, and o3 execute atomically
val repl3 = repl("i","o1","o2","o3")

```

We can visualized each of this connectors in Graphviz using: `toDot(repl2)`, `toDot(repl2var)`, `toDot(repl2varRes)` and `toDot(repl3)`:

![alt text](https://cdn.rawgit.com/joseproenca/ifta/master/src/main/scala/ifta/reo/images/reo-repl2.svg "IFTA view of a Replicator with 2 outpus and standard behavior")
![alt text](https://cdn.rawgit.com/joseproenca/ifta/master/src/main/scala/ifta/reo/images/reo-repl2-relax.svg "IFTA view of a Replicator with 2 outpus and variable behavior")
![alt text](https://cdn.rawgit.com/joseproenca/ifta/master/src/main/scala/ifta/reo/images/reo-repl2-relax-excludes.svg "IFTA view of a Replicator with 2 outpus and variable restricted behavior")
![alt text](https://cdn.rawgit.com/joseproenca/ifta/master/src/main/scala/ifta/reo/images/reo-repl3.svg "IFTA view of a Replicator with 3 outputs and standard behavior")
<!--### Merger-->

<!--The `merger` connector consists of two or more inputs `i1,i2,...` and an output `o`. In its standard behavior it synchronizes each input with the output port. If any of its inputs is missing, it synchronizes only each input present with the output. In addition, if the output is missing, it behaves as an `asyndrain` connector, enabling the execution of either of its inputs without further action. -->

<!--It can be specified as follows-->
<!--```scala-->
<!--// merger with 2 inputs-->
<!--val merger2 = merger("i1","i2","o")-->
<!--```-->
<!--and visualized in Graphviz using `toDot(merger2)`:-->

<!--![alt text](https://cdn.rawgit.com/joseproenca/ifta/master/src/main/scala/ifta/reo/images/merger.svg "Merger connector with 2 inputs modeled as IFTA")-->

### Composed connectors

The connectors provided in this folder can be used to construct more complex connectors using IFTA composition. 
We provide some examples in the file `ComplexConnectors` that can be found in the [examples folder](src/main/scala/ifta/examples). 
For example, a `sequencer of 3 outputs` is a connector that enables the execution of its outputs in a sequence fashion `o1,o2,o3,o1,o2,o3,...` , and it can be specified as follows. 

```scala
// Sequencer A>B>C as a NIFTA 
// where A, B and C are IFTA accepting inputs `a`, `b`, and `c`, respectively, defined in ComplexConnectors.scala
  val seq3net = 
    fifofull("i","o") ||
    (repl("o","a","o2").relax excludes "a,o2" when ("v_a" || "v_o2") --> "v_o") ||
    fifo("o2","o3") ||
    (repl("o3","b","o5").relax excludes "b,o5" when ("v_o5" || "v_b") --> "v_o3") ||
    fifo("o5","o6") ||
    (repl("o6","c","i").relax excludes "c,i"when ("v_i" || "v_c") --> "v_o6") ||
    A || B || C  when "v_i" <-> ("A" || "B" || "C") 
                
// Sequencer A>B>C as an IFTA:
  val seq3 = seq3net.flatten
```

We can see the top view of the network by generating the `dot graph` with the command `con2dot(seq3net)` and visualizing it using Graphiviz. The result is the following graph, where `[]`,`[.]`, and `-<` represent a `fifo1`, `fifo1full`, and `replicator` connector, respectively.

![alt text](https://cdn.rawgit.com/joseproenca/ifta/master/src/main/scala/ifta/reo/images/seq3net-top.svg "Top view of a sequencer of 3 outputs modeled as NIFTA")
