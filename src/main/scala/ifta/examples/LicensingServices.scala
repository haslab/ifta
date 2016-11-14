package ifta.examples

import ifta.DSL._
import ifta.{Edge, FTrue, IFTA}


/**
  * Created by guille on 27/10/16.
  */
object LicensingServices {

  ////////////////////////
  // Application Module //
  ////////////////////////

  val application = newifta ++ (
    0 --> 1 by "apply",
    1 --> 2 by "submitDocs",
    2 --> 5 by "submit" when not("pa") reset "tsub",
    2 --> 3 by "payapp" when "pa" reset "tpay",
    3 --> 4 by "paidapp" when "pa" reset "tpay",
    3 --> 0 by "cancelpay" when "pa" reset "tpay",
    4 --> 5 by "submit" when "pa" reset "tsub",
    5 --> 0 by "accept" reset "tsub",
    5 --> 0 by "incomplete"reset "tsub",
    5 --> 0 by "reject" when not("apl") reset "tsub",
    5 --> 6 by "reject" when "apl" reset "tapl,tsub",
    6 --> 7 by "appeal" when "apl" cc "tapl"<=30 reset "tapl",
    7 --> 0 by "reject" when "apl" reset "tapl",
    7 --> 0 by "accept" when "apl" reset "tapl",
    6 --> 0 by "cancelapp" when "apl" //cc "tapl"<=31
    ) get "paidapp,cancelpay,accept,reject,incomplete" pub "submit,payapp,appeal" inv(6,"tapl"<=31) name "App" ap (
    (2,"docs"),(3,"paying"),(5,"submitted"),(7,"appealed"),(4,"paid"),(6,"canAppeal"))

  /////////////////////
  //  Payment Module //
  /////////////////////

  val creditcard = newifta ++ (
    0 --> 1 by "paycc" when "cc" reset "toutcc",
    1 --> 0 by "cancelcc" when "cc",
    1 --> 0 by "paidcc" when "cc" //cc "toutcc">1 cc "toutcc"<=5
    ) startWith 0 get "paycc" pub "cancelcc,paidcc" inv(1,"toutcc"<=1) name "CC"

  val paypal = newifta ++ (
    0 --> 1 by "paypp" when "pp" reset "toutpp",
    1 --> 0 by "cancelpp" when "pp",
    1 --> 0 by "paidpp" when "pp" //cc "tsteppp">1 cc "tsteppp"<=5
    ) startWith 0 get "paypp" pub "cancelpp,paidpp" inv(1,"toutpp"<=1) name "PP"

  val paymentNet =
    router("payapp", "paycc", "paypp") ||
    paypal ||
    creditcard ||
    merger("cancelcc", "cancelpp", "cancelpay") ||
    merger("paidcc", "paidpp", "paidapp")

  ///////////////////////////////////
  // Processing Application Module //
  ///////////////////////////////////

  val appeal = newifta ++ (
    0 --> 1 by "appeal" when "apl" reset "tas",
    1 --> 0 by "assessapl" when "apl"
    ) startWith 0 get "appeal" pub "assessapl" inv(1,"tas"<=20) name "HandleApp"

  val assessment = newifta ++ (
    0 --> 1 by "assess" reset "tp",
    1 --> 0 by "accept",
    1 --> 0 by "reject"
    ) startWith 0 get "assess" pub "accept,reject" inv(1,"tp"<=90) name "Assess" ap(1,"assessing")

  val preAssessment = newifta ++ (
    0 --> 1 by "submit" reset "ts",
    1 --> 0 by "incomplete",
    1 --> 0 by "assessapp"
    ) inv(1,"ts"<=20) startWith 0 get "submit" pub "incomplete,assessapp" name "Preassess" ap (1,"checkingDocs")

  val processingNet =
    preAssessment || assessment || appeal || merger("assessapl","assessapp","assess")

  ////////////////////////////
  // Licensing Services SPL //
  ////////////////////////////

  val splNet =
    application || processingNet || paymentNet when "pa" <-> ("pp" || "cc")

  // IFTA composition
  val spl = splNet.flatten

  /**
    * splnet example queries:
    * A[] not deadlock
    * Liveness:
    * App.submitted --> App.L0  ----- (if submitted, eventually the user gets an answer)
    * App.appealed --> App.L0 ---- (if appeal submitted, eventually the user gets an answer)
    * App.paying --> (App.paid || App.L0) ---- (if paying, eventually paid or canceled)
    * Reachability:
    * E<>App.submitted
    * E<>App.appealed
    * E<>Assess.assessing
    * Safety:
    * A[] App.paying imply App.tpay <=1 ---- (It can no take longer than 1 day to pay or cancel)
    * A[] App.submitted imply App.tsub <=110 ----(if submitted the user gets an aswer in less than 110 days.
    * A[] App.appealed imply App.tapl <= 110 ----(if appealed the user gets an aswer in less than 110 days.
    *
    */
}
