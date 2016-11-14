# Examples of Interface Featured Timed Automata (IFTA)

This folder contain examples of IFTAs, including
 
 - A simple example of a [coffee machine]
 - A combination or Reo connectors (from the ones included in the DSL)
 - A more complex example from e-Gov domain for coordinating licensing services.

We explain better below the more complex example, submitted to an [FSEN'2017](http://fsen.ir/2017/) paper.


## Licensing Services: introduction

This case study uses IFTA to model a family of public licensing services. All services in the family support submissions of licensing requests, and assessment of requests. Some licensing services, in addition, require payments before submitting a request (feature `pa`), others allow appeals on rejected requests (feature `apl`), or both. Furthermore, services that require payments can support credit card (feature `cc`) or paypal payments (feature `pp`), or both. Functionality is divided in components and provided as follows.

This example uses 6 individual components: `Application`, `PayPall`, `CreditCard`, `Preassess`, `Appeeal`, and `Assess`. These components are combined via connectors that merge, route, and connect the interfaces of these components, totalizing 10 composed IFTA.

We follow a top-down approach; we first illustrate how the components are composed, followed by how each component is specified. Later we provide some conclusions obtained via the UPPAAL backend.

## Start scala and load the example

You only need [sbt](http://www.scala-sbt.org) and Java SDK installed. The easiest way to load the library is using the interperter, by executing the command ```sbt console```. You can then load the Licensing example:

```scala
import ifta.DSL._
import ifta.examples.LicensingServices._
```


## Connecting components with Reo

The final network of composed IFTAs is stored in variable `paymentnet`, combining the subparts of the network and adding the top-level restrictions over the features (can only have payment if there is either a Paypal or a Creditcard service).

```scala
// these definitions are inside LicensingServices.scala
(...)
val processingNet =
    preassessment || assessment || handleappeal ||
    merger("assessapl","assessapp","assess")
val paymentNet =
    paypal || creditcard ||
    router("payapp", "paycc", "paypp") ||
    merger("cancelcc", "cancelpp", "cancelpay") ||
    merger("paidcc", "paidpp", "paidapp")
val splNet =
    application || processingNet || paymentNet  when "pa" <-> ("pp" || "cc")
```

The top view of network can be visualised using Graphviz. For that execute `con2dot(paymentNet)` to produce the `dot` graph. This graph can be easily visualised, e.g., using the online tool [Viz.js](https://mdaines.github.io/viz.js/), producing the following graph. Here ">-" represent mergers and "Xor" represents the exclusive router.

![alt text](https://cdn.rawgit.com/joseproenca/ifta/master/src/main/ifta/examples/images/LS-conn.svg "Depicting the connector composing the Licensing Services.")


## Specifying components - primitive IFTAs

The automata of the 5 core components of these example can be visualised also using Graphviz, producing the graphs depicted below. For example, `toDot(application)` produces the first of the automata below.

![alt text](https://cdn.rawgit.com/joseproenca/ifta/master/src/main/ifta/examples/images/LS-application.svg "Application component of the Licensing Services.") ![alt text](https://cdn.rawgit.com/joseproenca/ifta/master/src/main/ifta/examples/images/LS-preassessment.svg "PreAssessment component of the Licensing Services.") ![alt text](https://cdn.rawgit.com/joseproenca/ifta/master/src/main/ifta/examples/images/LS-assessment.svg "Assessment component of the Licensing Services.") ![alt text](https://cdn.rawgit.com/joseproenca/ifta/master/src/main/ifta/examples/images/LS-handleappeal.svg "HandleAppeal component of the Licensing Services.") ![alt text](https://cdn.rawgit.com/joseproenca/ifta/master/src/main/ifta/examples/images/LS-paypal.svg "Paypal component of the Licensing Services.") ![alt text](https://cdn.rawgit.com/joseproenca/ifta/master/src/main/ifta/examples/images/LS-creditcard.svg "Creditcard component of the Licensing Services.")



## Model checking with UPPAAL

This example can be exported to UPPAAL in 2 different ways:
```scala
toUppaal(paymentNet        ,"pay1.xml")
toUppaal(paymentNet.flatten,"pay2.xml")
```

The first produces a network of 10 communicating timed automata, and the second produces a single timed automata obtained by composing the IFTAs.
The latter has no committed states (unlike the former), and has only 4 states (but 64 transitions), converted into a FTA by merging each set of multiple actions into an action with a concatenated name.

In both cases the automata contain feature annotations in their guards (since UPPAAL supports general boolean expressions in the guards), and each UPPAAL file includes an additional automata with the feature model.
This additional automata contains all possible selection of features, depiced below, and must be taken exactly once before any other automata, to capture the selection of the desired features before any action takes place.

![alt text](https://cdn.rawgit.com/joseproenca/ifta/master/src/main/ifta/examples/images/LS-fm.svg "Automata with the feature model of the Licensing Services.")

By loading the resulting files into UPPAAL, it is possible to verify properties such as:

 - Deadlock free – `A[] not deadlock`;
 - Liveness: a submition and an appeal will eventually result in an answer - `App.l4 --> App.l0` and `App.l6 --> App.l0`, respectively);
 - Safety: a submission must be processed within 110 days - `A[] App.l4 imply App.tsub <=110`;
 - Safety: a payment must be completed within 1 day of initiated - `A[] App.l2 imply App.tpay <=1`.
