package ifta.examples

import ifta.DSL._


/**
  * Created by guille on 27/10/16.
  */
object LicensingServices {

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

  val payment = (
    router("payapp", "paycc", "paypp")
    * paypal
    * creditcard
    * merger("canelcc", "cancelpp", "cancelpay")
    * merger("paidcc", "paidpp", "paidapp")
    )  

  ///////////////////////////////////
  // Processing Application Module //
  ///////////////////////////////////

  val handleappeal = newifta ++ (
    0 --> 1 by "getapl" when "apl",
    1 --> 0 by "assess" when "apl"
    ) startWith 0 get "getapl" pub "assess"

  val assessment = newifta ++ (
    0 --> 1 by "assess",
    1 --> 2 by "consultext" when ("tax" || "cr"),
    1 --> 3 by "decide",
    2 --> 4 by "waitext" when ("tax" || "cr"),
    4 --> 3 by "decide" when ("tax" || "cr"),
    3 --> 0 by "accept",
    3 --> 0 by "reject"
    ) startWith 0 get "assess,waitext" pub "consultext,accept,reject"

  val preassesment = newifta ++ (
    0 --> 1 by "getapp",
    1 --> 2 by "checkcompl",
    2 --> 3 by "incomplete",
    2 --> 4 by "complete",
    3 --> 0 by "reject",
    4 --> 0 by "assess"
    ) startWith 0 get "getapp" pub "reject,assess" 

  val processing = preassesment * assessment * handleappeal
  
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
  
  // consult tax, cr, or both
  val multchoice = (
    router("consultext","taxandcr","taxorcr") 
    || router("taxorcr", "ct","ccr")
    )

  // if only cr then don't wait for tax response
  val onlycr = (
    repl("ccr", "onlycr","onlycheckcr")
    || fifo("onlycr","notax")
    )

  // if only tax then don't wait for cr response
  val onlytax = (
    repl("ct", "onlytax","onlychecktax")
    || fifo("onlytax","nocr")
    )
  
  val taxandcr = repl("taxandcr","bothchecktax","bothcheckcr")

  val invoquetax = merger("onlychecktax","bothchecktax","checktax")

  val invoquecr = merger("onlycheckcr","bothcheckcr", "checkcr")

  // wait for all consulted DBs
  val wait4responses = (
    merger("notax","resptax","readytax")
    || merger("nocr","respcr","readycr")
    || join("readytax", "readycr","waitext")
    )

  val dbshandling = (
    multchoice || onlytax || onlycr || taxandcr ||
    invoquetax || invoquecr ||
    tax || criminalrecord ||
    wait4responses || (newifta when ("tax" || "cr"))
    )

  /**
    * Examples of properties I want to checked:
    * - always taxandcr --> eventually (resptax and respcr)
    * - always consultext --> eventually waitext
    * - always taxorcr --> eventually ((resptax and nocr) or (respcr and notax))
    */
}
