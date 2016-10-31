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
    2 --> 3 by "payapp",
    3 --> 4 by "paidapp",
    3 --> 0 by "cancelpay",
    4 --> 5 by "submit",
    5 --> 0 by "accept",
    5 --> 0 by "incomplete",
    5 --> 6 by "reject" when "apl" reset "tapl",
    5 --> 0 by "reject" when not("apl"),
    6 --> 7 by "appeal" when "apl" cc "tapl"<31,
    7 --> 0 by "reject" when "apl",
    7 --> 0 by "accept" when "apl",
    6 --> 0 by "cancelapp" when "apl" cc "tapl"<32
    ) get "paidapp,cancelpay,accept,reject,incomplete" pub "submit,payapp,appeal" inv(6,"tapl"<32)

  /////////////////////
  //  Payment Module //
  /////////////////////

  val creditcard = newifta ++ (
    0 --> 1 by "paycc" when "cc",
    1 --> 2 by "detailscc" when "cc",
    1 --> 0 by "cancelcc" when "cc",
    2 --> 1 by "errordcc" when "cc",
    2 --> 3 by "okdcc" when "cc",
    3 --> 4 by "confcc" when "cc",
    3 --> 0 by "cancelcc" when "cc",
    4 --> 3 by "errorpcc" when "cc",
    4 --> 5 by "okcc" when "cc",
    5 --> 0 by "paidcc" when "cc"
    ) startWith 0 get "paycc" pub "cancelcc,paidcc"

  val paypal = newifta ++ (
    0 --> 1 by "paypp" when "pp",
    1 --> 2 by "loginpp" when "pp",
    1 --> 0 by "cancelpp" when "pp",
    2 --> 1 by "errorlgpp" when "pp",
    2 --> 3 by "oklgpp" when "pp",
    3 --> 4 by "confpp" when "pp",
    3 --> 0 by "cancelpp" when "pp",
    4 --> 3 by "errorppp" when "pp",
    4 --> 5 by "okpp" when "pp",
    5 --> 0 by "paidpp" when "pp"
    ) startWith 0 get "paypp" pub "cancelpp,paidpp"

  val payment = (router("payapp", "paycc", "paypp") ||
    paypal ||
    creditcard ||
    merger("cancelcc", "cancelpp", "cancelpay") ||
    merger("paidcc", "paidpp", "paidapp") || (newifta when ("pp" || "cc")))

  ///////////////////////////////////
  // Processing Application Module //
  ///////////////////////////////////

  val handleappeal = newifta ++ (
    0 --> 1 by "appeal" when "apl",
    1 --> 0 by "assess" when "apl"
    ) startWith 0 get "appeal" pub "assess"

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
    0 --> 1 by "assess",
    1 --> 2 by "decide",
    2 --> 0 by "accept",
    2 --> 0 by "reject"
    ) startWith 0 get "assess" pub "accept,reject"

  val preassesment = newifta ++ (
    0 --> 1 by "submit" reset "ts",
    1 --> 2 by "checkcompl",
    2 --> 0 by "incomplete",
    2 --> 3 by "complete",
    // 3 --> 0 by "reject",
    3 --> 0 by "assess"
    ) inv(1,"ts"<21) startWith 0 get "submit" pub "incomplete,assess"

  val processing = preassesment || assessment || handleappeal


  ////////////////////////////
  // Licensing Services SPL //
  ////////////////////////////

  val spl = application || processing || payment

  /**
    * Properties checked:
    * A[] not deadlock: ok
    * A<> FTA_0.L3 imply (FTA_4.L1 or FTA_7.L1): ok ( If payapp in appplication then eventually ( paypal.paypp or creditcard.paycc))
    * A<> FTA_0.L5 imply FTA_0.L0: of (if appplication.submit then eventually I receive some answer and I'm able to start a new submision again)
    * A<> FTA_0.L0 and FTA_5.L0: ok (if a can start an application then it is awalys the case that there is no processing in process)
    */

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


  /**
    * Examples of properties I want to checked:
    * - always taxandcr --> eventually (resptax and respcr)
    * - always consultext --> eventually waitext
    * - always taxorcr --> eventually ((resptax and nocr) or (respcr and notax))
    */

  //////////////////////////////
  // test without variability //
  //////////////////////////////

  def v(s:String) = "v"+s

  def nvrouter(i:String,o1:String,o2:String) = newifta ++ (
    0 --> 0 by s"$i,$o1",
    0 --> 0 by s"$i,$o2"
    ) get i pub s"$o1,$o2" when v(i) && v(o1) && v(o2)

  def nvrouter3(i:String,o1:String,o2:String,o3:String) = newifta ++ (
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

  def nvjoin3(i:String,i2:String,i3:String,o:String) = newifta ++ (
    0 --> 0 by s"$i,$i2,$i3,$o"
    ) get i get i2 get i3 pub o when v(i) && v(i2) && v(i3) && v(o)

  def nvjoin4(i:String,i2:String,i3:String,i4:String,o:String) = newifta ++ (
    0 --> 0 by s"$i,$i2,$i3,$i4,$o"
    ) get i get i2 get i3 get i4 pub o  when v(i) && v(i2) && v(i3) && v(i4) && v(o)

  def nvmerger(i:String, i2:String, o:String) = newifta ++ (
    0 --> 0 by s"$i,$o",
    0 --> 0 by s"$i2,$o"
    ) get i get i2 pub o when v(i) && v(i2) && v(o)

  def nvmerger3(i:String, i2:String,i3:String, o:String) = newifta ++ (
    0 --> 0 by s"$i,$o",
    0 --> 0 by s"$i2,$o",
    0 --> 0 by s"$i3,$o"
    ) get i get i2 get i3 pub o when v(i) && v(i2) && v(i3) && v(o)

  def nvmerger4(i:String, i2:String,i3:String,i4:String,o:String) = newifta ++ (
    0 --> 0 by s"$i,$o",
    0 --> 0 by s"$i2,$o",
    0 --> 0 by s"$i3,$o",
    0 --> 0 by s"$i4,$o"
    ) get i get i2 get i3 get i4 pub o when v(i) && v(i2) && v(i3) && v(i4) && v(o)

  def nvrepl(i:String,o1:String,o2:String) = newifta ++ (
    0 --> 0 by s"$i,$o1,$o2"
    ) get i pub o1 pub o2 when v(i) && v(o1) && v(o2)

  def nvrepl3(i:String,o1:String,o2:String,o3:String) = newifta ++ (
    0 --> 0 by s"$i,$o1,$o2,$o3"
    ) get i pub o1 pub o2 pub o3 when v(i) && v(o1) && v(o2) && v(o3)

  def nvrepl4(i:String,o1:String,o2:String,o3:String,o4:String) = newifta ++ (
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

  val repl1 = nvrepl("consultext","repl1o1","repl1o2")
  
  val router1 = nvrouter3("repl1o1","router1o1","router1o2","router1o3")

  val fifo1 = nvfifo("repl1o2","fifo1o")

  val repl2 = nvrepl("router1o1","repl2o1","repl2o2")

  val repl3 = nvrepl4("router1o2","repl3o1", "repl3o2","repl3o3","repl3o4")

  val repl4 = nvrepl("router1o3","repl4o1","repl4o2")

  val sdrain1 = nvsdrain("fifo1o","sdrain1i2")

  val fifo2 = nvfifo("repl2o1","fifo2o")

  val merger1 = nvmerger("repl2o2","repl3o1","checktax")

  val fifo3 = nvfifo("repl3o2","fifo3o")

  val fifo4 = nvfifo("repl3o3","fifo4o")

  val merger2 = nvmerger("repl3o4","repl4o1","checkcr")

  val fifo5 = nvfifo("repl4o2","fifo5o")

  val repl5 = nvrepl3("fifo2o","repl5o1","repl5o2","repl5o3")

  val sdrain2 = nvsdrain("repl5o3","router2o1")

  val router2 = nvrouter("resptax","router2o1","router2o2")

  val sdrain3 = nvsdrain("router2o2","repl6o1")

  val repl6 = nvrepl3("fifo3o","repl6o1","repl6o2","repl6o3")

  val repl7 = nvrepl3("fifo4o","repl7o1","repl7o2","repl7o3")  

  val sdrain4 = nvsdrain("repl7o3","router3o1")

  val router3 = nvrouter("respcr","router3o1","router3o2")

  val sdrain5 = nvsdrain("router3o2","repl8o1")

  val repl8 = nvrepl3("fifo5o","repl8o1","repl8o2","repl8o3")

  val fifo6 = nvfifo("repl6o2","fifo6o")

  val fifo7 = nvfifo("repl7o2","fifo7o")

  val sdrain6 = nvsdrain("fifo6o","repl9o1")

  val repl9 = nvrepl("fifo7o","repl9o1","repl9o2")

  val merger3 = nvmerger4("repl5o1","repl6o3","repl7o1","repl8o2","readyext")

  val merger4 = nvmerger3("repl9o2","repl5o2","repl8o3","sdrain1i2")

  val context = newifta ++ (
    0 --> 1 by "consultext",
    1 --> 0 by "readyext"
    ) startWith 0 get "readyext" pub "consultext"

  val syncmergernetwork =
    repl1 || repl2 || repl3 || repl4 || repl5 || repl6 || repl7 || repl8 || repl9 ||
    router1 || router2 || router3 ||
    fifo1 || fifo2 || fifo3 || fifo4 || fifo5 || fifo6 || fifo7 ||
    sdrain1 || sdrain2 || sdrain3 || sdrain4 || sdrain5 || sdrain6 ||
    merger1 || merger2 || merger3 || merger4 ||
    nvtax || nvcriminalrecord

  val syncmerger =
    repl1 * repl2 * repl3 * repl4 * repl5 * repl6 * repl7 * repl8 * repl9 *
    router1 * router2 * router3 *
    fifo1 * fifo2 * fifo3 * fifo4 * fifo5 * fifo6 * fifo7 *
    sdrain1 * sdrain2 * sdrain3 * sdrain4 * sdrain5 * sdrain6 *
    merger1 * merger2 * merger3 * merger4 *
    nvtax * nvcriminalrecord
}
