# Reo connectors as Interface Featured Timed Automata (IFTA)

This folder contains IFTA constructors for some common Reo connectors, including: `sync`, `syncDrain`, `asyncDrain`, `fifo1`, `fifo1full`, `router`, `replicator`, `merger`, and `join`.
 
These constructors can be used to build more complex orchestration mechanism by means of IFTA composition.

They can be used by importing the corresponding class:
```scala
import ifta.reo.Connectors._
```
<!--## Semantics of Reo Connectors-->

## Reo

Reo is a channel-based exogenous coordination language where complex coordinators, called connectors, are compositionally built out of simpler ones, called channels. Exogenous coordination facilitates anonymous communication of components. Each connector has a set of input and output ports, and a formal semantics of how data flows from the inputs to the outputs. For our purpose, we abstract from the notion of data and rather concentrate on how execution of actions associated to input ports enable execution of actions associated to output ports.

## Using IFTA Connectors
Modeling Reo connectors as IFTA enables them with variable behavior based on the presence of ports connected, through synchronization, to their ports. 
We explain the semantics of some connectors in terms of their standard and variable behavior, and we show how they can be constructed, composed and visualized using the provided DSL. 

### Sync
The `sync` connector consists of an input `i` and an output `o` port. In its standard behavior, it synchronizes its input port `i` with its output port `o`, meaning they must execute atomically. If the output is not present, it behaves as an `asyncDrain` connector (see below), receiving inputs without further action.  

It can be specified as follows
```scala
val mysync = sync("i","o")
```
and visualized in Graphviz using `toDot(mysync)`:

![alt text](https://cdn.rawgit.com/joseproenca/ifta/master/src/main/scala/ifta/reo/images/sync.svg "Sync connector as IFTA")

### SyncDrain
The `syncDrain` connector consists of two inputs `i1` and `i2`. In its standard behavior, both inputs must execute atomically. If an input is missing, it behaves as and `asyncDrain` connector, receiving inputs without further action.

It can be specified as follows
```scala
val mysdrain = sdrain("i1","i2")
```
and visualized in Graphviz using `toDot(mysdrain)`:

![alt text](https://cdn.rawgit.com/joseproenca/ifta/master/src/main/scala/ifta/reo/images/sdrain.svg "SyncDrain connector as IFTA")

### AsyncDrain

The `asyncDrain` connector consists of two inputs `i1` and `i2`. Receives inputs in either of its ports without further action. 

It can be specified as follows
```scala
val myasdrain = asdrain("i1","i2")
```
and visualized in Graphviz using `toDot(myasdrain)`:

![alt text](https://cdn.rawgit.com/joseproenca/ifta/master/src/main/scala/ifta/reo/images/asdrain.svg "AsyncDrain connector as IFTA")


### Replicator 

The `replicator` connector consists of an input `i` and two or more outputs `o1,o2,...`. In its standard behavior it synchronizes all inputs with the output port, i.e., they all must execute atomically. If any of its inputs is missing, only the inputs that are present are synchonized with the output. In addition, if the output is missing, it behaves as a `syndrain` synchronizing the execution of all the inputs that are present. 

It can be specified as follows
```scala
// replicator with 2 outputs
val repl2 = repl("i","o1","2")

// replicator with 3 outputs
val repl3 = repl("i","o1","o2","o3")
```

and visualized in Graphviz using `toDot(repl2)` and `toDot(repl4)`:

![alt text](https://cdn.rawgit.com/joseproenca/ifta/master/src/main/scala/ifta/reo/images/repl.svg "Replicator connector with 2 outputs modeled as IFTA")
![alt text](https://cdn.rawgit.com/joseproenca/ifta/master/src/main/scala/ifta/reo/images/repl3.svg "Replicator connector with 4 outputs modeled as IFTA")

### Merger

The `merger` connector consists of two or more inputs `i1,i2,...` and an output `o`. In its standard behavior it synchronizes each input with the output port. If any of its inputs is missing, it synchronizes only each input present with the output. In addition, if the output is missing, it behaves as an `asyndrain` connector, enabling the execution of either of its inputs without further action. 

It can be specified as follows
```scala
// merger with 2 inputs
val merger2 = merger("i1","i2","o")
```
and visualized in Graphviz using `toDot(merger2)`:

![alt text](https://cdn.rawgit.com/joseproenca/ifta/master/src/main/scala/ifta/reo/images/merger.svg "Merger connector with 2 inputs modeled as IFTA")

### Composed connectors

The connectors provided can be used to construct more complex connectors using IFTA composition. We provide some examples in the file `ComplexConnectors` that can be found in the [examples folder](src/main/scala/ifta/examples). For example, a `sequencer of 3 outputs` is a connector that enables the execution of its outputs in a sequence fashion `o1,o2,o3,o1,o2,o3,...` , and it can be specified as follows. 

```scala
//  Sequencer a>b>c as a network of IFTA:
 val seq3net =
     fifofull("i","o") || repl("o","a","o2") ||
       fifo("o2","o3") || repl("o3","b","o5") ||
       fifo("o5","o6") || repl("o6","c","i")
    
  // equencer a>b>c as an IFTA:
  val seq3 = seq3net.flatten
```

We can see the top view of the network by generating the `dot graph` with the command `con2dot(seq3net)` and visualizing it using Graphiviz. The result is the following graph, where `[]`,`[.]`, and `-<` represent a `fifo1`, `fifo1full`, and `replicator` connector, respectively.

![alt text](https://cdn.rawgit.com/joseproenca/ifta/master/src/main/scala/ifta/reo/images/seq3net-top.svg "Top view of a sequencer of 3 outputs modeled as NIFTA")






