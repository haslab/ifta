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
    2 --> 5 by "submit" when not("pa"),
    2 --> 3 by "payapp" when "pa",
    3 --> 4 by "paidapp" when "pa",
    3 --> 0 by "cancelpay" when "pa",
    4 --> 5 by "submit" when "pa",
    5 --> 0 by "accept",
    5 --> 0 by "incomplete",
    5 --> 0 by "reject" when not("apl"),
    5 --> 6 by "reject" when "apl" reset "tapl",
    6 --> 7 by "appeal" when "apl" cc "tapl"<=30,
    7 --> 0 by "reject" when "apl",
    7 --> 0 by "accept" when "apl",
    6 --> 0 by "cancelapp" when "apl" cc "tapl"<=31
    ) get "paidapp,cancelpay,accept,reject,incomplete" pub "submit,payapp,appeal" inv(6,"tapl"<=31) name "App"

  // Things to check:
  // if submit then eventually I get an answer (go back to 0)
  // if I appeal then eventually I get an answer
  // If payapp then eventually paidapp or cancelpay
  // if I submitt I should be back to 0 with processing time <= x
  // if appeal then I should b back to 0 with processing time <= x
  //(FTA_5.L5 && apl) --> (FTA_5.L0 &&  FTA_9.ts <=20) || (FTA_5.L0 && FTA_8.tp<=90) || (FTA_5.L0 && FTA_8.tp<=90+31)
  //(FTA_5.L5 && !apl) --> (FTA_5.L0 &&  FTA_9.ts <=20) || (FTA_5.L0 && FTA_8.tp<=90)
  //FTA_5.L6 --> (FTA_5.L0 && FTA_5.tapl<=31) || (FTA_5.L7 && FTA_5.tapl<=30)
  //FTA_5.L7 --> (FTA_5.L0)
  //FTA_5.L6 --> (FTA_5.L7 || FTA_5.L0)

  /////////////////////
  //  Payment Module //
  /////////////////////

//  val creditcard = newifta ++ (
//    0 --> 1 by "paycc" when "cc" reset "tpay",
//    1 --> 2 by "detailscc" when "cc" cc "tpay"<=1 reset "tpay",
//    1 --> 0 by "cancelcc" when "cc",
//    2 --> 3 by "confcc" when "cc" cc "tpay"<=1 reset "tpay",
//    2 --> 0 by "cancelcc" when "cc",
//    3 --> 0 by "paidcc" when "cc" cc "tpay"<=1,
//    3 --> 0 by "cancelcc" when "cc" cc "tpay">1
//    ) startWith 0 get "paycc" pub "cancelcc,paidcc" inv(1,"tpay"<=2) inv(2,"tpay"<=2) inv(3,"tpay"<=2) name "CC"

//  val paypal = newifta ++ (
//    0 --> 1 by "paypp" when "pp" reset "tpay",
//    1 --> 2 by "loginpp" when "pp" reset "tpay" cc "tpay"<=1,
//    1 --> 0 by "cancelpp" when "pp",
//    2 --> 3 by "confpp" when "pp" cc "tpay"<=1 reset "tpay",
//    2 --> 0 by "cancelpp" when "pp",
//    3 --> 0 by "paidpp" when "pp" cc "tpay"<=1,
//    3 --> 0 by "cancelpp" when "pp" cc "tpay">1
//    ) startWith 0 get "paypp" pub "cancelpp,paidpp" inv(1,"tpay"<=2) inv(2,"tpay"<=2) inv(3,"tpay"<=2) name "PP"

  val creditcard = newifta ++ (
    0 --> 1 by "paycc" when "cc" reset "toutcc,tstepcc",
    1 --> 2 by "detailscc" when "cc" cc "tstepcc">1 cc "tstepcc"<=5 reset "tstepcc",
    1 --> 0 by "cancelcc" when "cc",
    2 --> 0 by "cancelcc" when "cc",
    2 --> 0 by "paidcc" when "cc" cc "tstepcc">1 cc "tstepcc"<=5
    ) startWith 0 get "paycc" pub "cancelcc,paidcc" inv(1,"toutcc"<=15) inv(2,"toutcc"<=15) name "CC"

  val paypal = newifta ++ (
    0 --> 1 by "paypp" when "pp" reset "tsteppp,toutpp",
    1 --> 2 by "loginpp" when "pp" cc "tsteppp">1 cc "tsteppp"<=5 reset "tsteppp" ,
    1 --> 0 by "cancelpp" when "pp",
    2 --> 0 by "cancelpp" when "pp",
    2 --> 0 by "paidpp" when "pp" cc "tsteppp">1 cc "tsteppp"<=5
    ) startWith 0 get "paypp" pub "cancelpp,paidpp" inv(1,"toutpp"<=15) inv(2,"toutpp"<=15) name "PP"

  val paymentnet = (router("payapp", "paycc", "paypp") ||
    paypal ||
    creditcard ||
    merger("cancelcc", "cancelpp", "cancelpay") ||
    merger("paidcc", "paidpp", "paidapp")) //|| (newifta when ("pp" || "cc")))

  val payment = (router("payapp", "paycc", "paypp") *
    paypal *
    creditcard *
    merger("cancelcc", "cancelpp", "cancelpay") *
    merger("paidcc", "paidpp", "paidapp") 
    ) //when ("pp" || "cc")


  ///////////////////////////////////
  // Processing Application Module //
  ///////////////////////////////////

  val handleappeal = newifta ++ (
    0 --> 1 by "appeal" when "apl" reset "tas",
    1 --> 0 by "assessapl" when "apl"
    ) startWith 0 get "appeal" pub "assessapl" inv(1,"tas"<=20) name "handleApp"

//  val assessment = newifta ++ (
//    0 --> 1 by "assess",
//    1 --> 2 by "consultext" when ("tax" || "cr"),
//    1 --> 3 by "decide",
//    2 --> 4 by "waitext" when ("tax" || "cr"),
//    4 --> 3 by "decide" when ("tax" || "cr"),
//    3 --> 0 by "accept",
//    3 --> 0 by "reject"
//    ) startWith 0 get "assess,waitext" pub "consultext,accept,reject"

  val assessment = newifta ++ (
    0 --> 1 by "assess" reset "tp",
    1 --> 0 by "accept",
    1 --> 0 by "reject"
    ) startWith 0 get "assess" pub "accept,reject" inv(1,"tp"<=90) name "assess"

  val preassessment = newifta ++ (
    0 --> 1 by "submit" reset "ts",
    1 --> 0 by "incomplete",
    1 --> 0 by "assessapp"
    ) inv(1,"ts"<=20) startWith 0 get "submit" pub "incomplete,assessapp" name "preassess"

  val processingnet = preassessment || assessment || handleappeal || merger("assessapl","assessapp","assess")

  val processing = preassessment * assessment * handleappeal * merger("assessapl","assessapp","assess")


  ////////////////////////////
  // Licensing Services SPL //
  ////////////////////////////

  val splnet = application || processingnet || paymentnet || (newifta when "pa" <-> ("pp" || "cc"))

  val spl = application * processing * payment when "pa" <-> ("pp" || "cc")


  ////////////////////////////////////
  // Consulting External DBs Module //
  ////////////////////////////////////

  // DB API for tax consultation
  val tax = newifta ++ (
    0 --> 1 by "checktax" when "tax",
    1 --> 0 by "resptax" when "tax"
    ) startWith 0 get "checktax" pub "resptax"

  // DB API for criminal record consultation
  val criminalrecord = newifta ++ (
    0 --> 1 by "checkcr" when "cr",
    1 --> 0 by "respcr" when "cr"
    ) startWith 0 get "checkcr" pub "respcr"

//   Examples of properties I want to checked:
//  - always taxandcr --> eventually (resptax and respcr)
//  - always consultext --> eventually waitext
//  - always taxorcr --> eventually ((resptax and nocr) or (respcr and notax))

  val repl1 = repl("consultext","repl1o1","repl1o2")

  val router1 = router("repl1o1","router1o1","router1o2","router1o3")

  val fifo1 = fifo("repl1o2","fifo1o")

  val repl2 = repl("router1o1","repl2o1","repl2o2")

  val repl3 = repl("router1o2","repl3o1", "repl3o2","repl3o3","repl3o4")

  val repl4 = repl("router1o3","repl4o1","repl4o2")

  val sdrain1 = sdrain("fifo1o","sdrain1i2")

  val fifo2 = fifo("repl2o1","fifo2o")

  val merger1 = merger("repl2o2","repl3o1","checktax")

  val fifo3 = fifo("repl3o2","fifo3o")

  val fifo4 = fifo("repl3o3","fifo4o")

  val merger2 = merger("repl3o4","repl4o1","checkcr")

  val fifo5 = fifo("repl4o2","fifo5o")

  val repl5 = repl("fifo2o","repl5o1","repl5o2","repl5o3")

  val sdrain2 = sdrain("repl5o3","router2o1")

  val router2 = router("resptax","router2o1","router2o2")

  val sdrain3 = sdrain("router2o2","repl6o1")

  val repl6 = repl("fifo3o","repl6o1","repl6o2","repl6o3")

  val repl7 = repl("fifo4o","repl7o1","repl7o2","repl7o3")

  val sdrain4 = sdrain("repl7o3","router3o1")

  val router3 = router("respcr","router3o1","router3o2")

  val sdrain5 = sdrain("router3o2","repl8o1")

  val repl8 = repl("fifo5o","repl8o1","repl8o2","repl8o3")

  val fifo6 = fifo("repl6o2","fifo6o")

  val fifo7 = fifo("repl7o2","fifo7o")

  val sdrain6 = sdrain("fifo6o","repl9o1")

  val repl9 = repl("fifo7o","repl9o1","repl9o2")

  val merger3 = merger("repl5o1","repl6o3","repl7o1","repl8o2","readyext")

  val merger4 = merger("repl9o2","repl5o2","repl8o3","sdrain1i2")

  lazy val syncmergernetwork = (
    repl1 || repl2 || repl3 || repl4 || repl5 || repl6 || repl7 || repl8 || repl9 ||
      router1 || router2 || router3 ||
      fifo1 || fifo2 || fifo3 || fifo4 || fifo5 || fifo6 || fifo7 ||
      sdrain1 || sdrain2 || sdrain3 || sdrain4 || sdrain5 || sdrain6 ||
      merger1 || merger2 || merger3 || merger4 ||
      tax || criminalrecord || (newifta when ("tax" || "cr") || not("tax" || "cr")))
//
  lazy val syncmerger = (
    repl1 * repl2 * repl3 * repl4 * repl5 * repl6 * repl7 * repl8 * repl9 *
      router1 * router2 * router3 *
      fifo1 * fifo2 * fifo3 * fifo4 * fifo5 * fifo6 * fifo7 *
      sdrain1 * sdrain2 * sdrain3 * sdrain4 * sdrain5 * sdrain6 *
      merger1 * merger2 * merger3 * merger4 *
      tax * criminalrecord  when ("tax" || "cr") || not("tax" || "cr"))


  //////////////////////////////
  // test without variability //
  //////////////////////////////

   def v(s:String) = "v"+s
  
  def nvrouter(i:String,o1:String,o2:String) = newifta ++ (
    0 --> 0 by s"$i,$o1",
    0 --> 0 by s"$i,$o2"
    ) get i pub s"$o1,$o2" when v(i) && v(o1) && v(o2)

  def nvroutern3(i:String,o1:String,o2:String,o3:String) = newifta ++ (
   0 --> 0 by s"$i,$o1",
     0 --> 0 by s"$i,$o2",
     0 --> 0 by s"$i,$o3"
     ) get i pub s"$o1,$o2,$o3" when v(i) && v(o1) && v(o2) && v(o3)

   def nvfifo(i:String,o:String) = newifta ++ (
     0 --> 1 by s"$i",
     1 --> 0 by s"$o"
     ) get i pub o when v(i) && v(o)

    def nvjoin(i:String,i2:String,o:String) = newifta ++ (
     0 --> 0 by s"$i,$i2,$o"
     ) get i get i2 pub o when v(i) && v(i2) && v(o)

   def nvjoinn3(i:String,i2:String,i3:String,o:String) = newifta ++ (
     0 --> 0 by s"$i,$i2,$i3,$o"
     ) get i get i2 get i3 pub o when v(i) && v(i2) && v(i3) && v(o)

   def nvjoinn4(i:String,i2:String,i3:String,i4:String,o:String) = newifta ++ (
     0 --> 0 by s"$i,$i2,$i3,$i4,$o"
     ) get i get i2 get i3 get i4 pub o  when v(i) && v(i2) && v(i3) && v(i4) && v(o)

   def nvmerger(i:String, i2:String, o:String) = newifta ++ (
     0 --> 0 by s"$i,$o",
     0 --> 0 by s"$i2,$o"
     ) get i get i2 pub o when v(i) && v(i2) && v(o)

   def nvmergern3(i:String, i2:String,i3:String, o:String) = newifta ++ (
     0 --> 0 by s"$i,$o",
     0 --> 0 by s"$i2,$o",
     0 --> 0 by s"$i3,$o"
     ) get i get i2 get i3 pub o when v(i) && v(i2) && v(i3) && v(o)

   def nvmergern4(i:String, i2:String,i3:String,i4:String,o:String) = newifta ++ (
     0 --> 0 by s"$i,$o",
     0 --> 0 by s"$i2,$o",
     0 --> 0 by s"$i3,$o",
     0 --> 0 by s"$i4,$o"
     ) get i get i2 get i3 get i4 pub o when v(i) && v(i2) && v(i3) && v(i4) && v(o)

   def nvrepl(i:String,o1:String,o2:String) = newifta ++ (
     0 --> 0 by s"$i,$o1,$o2"
     ) get i pub o1 pub o2 when v(i) && v(o1) && v(o2)

   def nvrepln3(i:String,o1:String,o2:String,o3:String) = newifta ++ (
     0 --> 0 by s"$i,$o1,$o2,$o3"
     ) get i pub o1 pub o2 pub o3 when v(i) && v(o1) && v(o2) && v(o3)

   def nvrepln4(i:String,o1:String,o2:String,o3:String,o4:String) = newifta ++ (
     0 --> 0 by s"$i,$o1,$o2,$o3,$o4"
     ) get i pub o1 pub o2 pub o3 pub o4 when v(i) && v(o1) && v(o2) && v(o3) && v(o4)

   def nvsdrain(i:String,i2:String) = newifta ++ (
     0 --> 0 by s"$i,$i2" when v(i) && v(i2)
     ) get i get i2 when v(i) && v(i2)

   val nvtax = newifta ++ (
     0 --> 1 by "checktax",
     1 --> 0 by "resptax"
     ) startWith 0 get "checktax" pub "resptax"

   // DB API for criminal record consultation
   val nvcriminalrecord = newifta ++ (
     0 --> 1 by "checkcr",
     1 --> 0 by "respcr"
     ) startWith 0 get "checkcr" pub "respcr"

   val nvrepl1 = nvrepl("consultext","repl1o1","repl1o2")

   val nvrouter1 = nvroutern3("repl1o1","router1o1","router1o2","router1o3")

   val nvfifo1 = nvfifo("repl1o2","fifo1o")

   val nvrepl2 = nvrepl("router1o1","repl2o1","repl2o2")

   val nvrepl3 = nvrepln4("router1o2","repl3o1", "repl3o2","repl3o3","repl3o4")

   val nvrepl4 = nvrepl("router1o3","repl4o1","repl4o2")

   val nvsdrain1 = nvsdrain("fifo1o","sdrain1i2")

   val nvfifo2 = nvfifo("repl2o1","fifo2o")

   val nvmerger1 = nvmerger("repl2o2","repl3o1","checktax")

   val nvfifo3 = nvfifo("repl3o2","fifo3o")

   val nvfifo4 = nvfifo("repl3o3","fifo4o")

   val nvmerger2 = nvmerger("repl3o4","repl4o1","checkcr")

   val nvfifo5 = nvfifo("repl4o2","fifo5o")

   val nvrepl5 = nvrepln3("fifo2o","repl5o1","repl5o2","repl5o3")

   val nvsdrain2 = nvsdrain("repl5o3","router2o1")

   val nvrouter2 = nvrouter("resptax","router2o1","router2o2")

   val nvsdrain3 = nvsdrain("router2o2","repl6o1")

   val nvrepl6 = nvrepln3("fifo3o","repl6o1","repl6o2","repl6o3")

   val nvrepl7 = nvrepln3("fifo4o","repl7o1","repl7o2","repl7o3")

   val nvsdrain4 = nvsdrain("repl7o3","router3o1")

   val nvrouter3 = nvrouter("respcr","router3o1","router3o2")

   val nvsdrain5 = nvsdrain("router3o2","repl8o1")

   val nvrepl8 = nvrepln3("fifo5o","repl8o1","repl8o2","repl8o3")

   val nvfifo6 = nvfifo("repl6o2","fifo6o")

   val nvfifo7 = nvfifo("repl7o2","fifo7o")

   val nvsdrain6 = nvsdrain("fifo6o","repl9o1")

   val nvrepl9 = nvrepl("fifo7o","repl9o1","repl9o2")

   val nvmerger3 = nvmergern4("repl5o1","repl6o3","repl7o1","repl8o2","readyext")

   val nvmerger4 = nvmergern3("repl9o2","repl5o2","repl8o3","sdrain1i2")

   val nvcontext = newifta ++ (
     0 --> 1 by "consultext",
     1 --> 0 by "readyext"
     ) startWith 0 get "readyext" pub "consultext"

   lazy val nvsyncmergernetwork =
     nvrepl1 || nvrepl2 || nvrepl3 || nvrepl4 || nvrepl5 || nvrepl6 || nvrepl7 || nvrepl8 || nvrepl9 ||
     nvrouter1 || nvrouter2 || nvrouter3 ||
     nvfifo1 || nvfifo2 || nvfifo3 || nvfifo4 || nvfifo5 || nvfifo6 || nvfifo7 ||
     nvsdrain1 || nvsdrain2 || nvsdrain3 || nvsdrain4 || nvsdrain5 || nvsdrain6 ||
     nvmerger1 || nvmerger2 || nvmerger3 || nvmerger4 ||
     nvtax || nvcriminalrecord

   lazy val nvsyncmerger =
     nvrepl1 * nvrepl2 * nvrepl3 * nvrepl4 * nvrepl5 * nvrepl6 * nvrepl7 * nvrepl8 * nvrepl9 *
     nvrouter1 * nvrouter2 * nvrouter3 *
     nvfifo1 * nvfifo2 * nvfifo3 * nvfifo4 * nvfifo5 * nvfifo6 * nvfifo7 *
     nvsdrain1 * nvsdrain2 * nvsdrain3 * nvsdrain4 * nvsdrain5 * nvsdrain6 *
     nvmerger1 * nvmerger2 * nvmerger3 * nvmerger4 *
     nvtax * nvcriminalrecord
}
