package ifta.analyse

import ifta._

import scala.util.parsing.combinator._

/**
  * Created by guille on 17/01/2019
  */


object Parser extends RegexParsers {

  def parseFexp(fe:String):ParseResult[FExp] = parseAll(fexp,fe)

  def parseEfficientFexp(fe:String):ParseResult[FExp] = parseAll(efexp,fe)

  def parseProducts(prod:String):ParseResult[Set[Set[String]]] = parseAll(products,prod)

  def parseFeats(feats:String):ParseResult[Set[String]] = parseAll(prod,feats)

  override def skipWhitespace = true

  /* Feature Expression */

  val feat: Parser[FExp] = """[a-zA-Z][a-zA-Z0-9_]*""".r ^^ {case f => Feat(f)}
  val top: Parser[FExp] = /*true*/ """⊤""".r ^^ {_ => FTrue}
  val bottom:Parser[FExp] = /*!true*/ """⊥""".r ^^ {_ => FNot(FTrue)}

  def fexp:Parser[FExp] =
    leftFexp ~ binOp ~ fexp ^^ {case f ~ op ~ f1 => op(f,f1)} |
    leftFexp

  def leftFexp:Parser[FExp] =
    feat |
    bottom |
    top |
    "!"~feat    ^^ {case _~f => FNot(f)} |
    "!"~parFexp ^^ {case _~f => FNot(f)} |
    parFexp
  //"("~>fexp<~")"
  //fexp

  def parFexp: Parser[FExp] =
    "("~>fexp<~")"

  def binOp: Parser[(FExp,FExp) => FExp] =
  "&"  ^^ { _ => (f1:FExp,f2:FExp) => f1 && f2}  |
  "|"  ^^ { _ => (f1:FExp,f2:FExp) => f1 || f2}  |
  "-->" ^^ { _ => (f1:FExp,f2:FExp) => f1 --> f2} |
  "<->" ^^ { _ => (f1:FExp,f2:FExp) => f1 <-> f2}

  /* Efficient Feature Expression */

  def efexp:Parser[FExp] =
  "Feat("~>feat<~")" |
  "FAnd("~ efexp ~","~ efexp ~")" ^^ {case _~e1~_~e2~_ => FAnd(e1,e2)}  |
  "FOr("~ efexp ~","~ efexp ~")" ^^ {case _~e1~_~e2~_ => FOr(e1,e2)}  |
  "FNot("~ efexp ~")" ^^ {case _ ~ e ~ _  => FNot(e)} |
  "FEq("~ efexp ~","~ efexp ~")" ^^ {case _~e1~_~e2~_ => FEq(e1,e2)}  |
  "FImp("~ efexp ~","~ efexp ~")" ^^ {case _~e1~_~e2~_ => FImp(e1,e2)}


  /* Set of Products */

  val featName:Parser[String] = """[a-zA-Z][a-zA-Z0-9_]*""".r ^^ { _.toString }

  def products: Parser[Set[Set[String]]] =
    "("~prod~","~moreProd~")" ^^ {case _~p~_~set~_ => set + p} |
    "("~prod~")" ^^ {case _~p~_  => Set(p)}

  def prod: Parser[Set[String]] =
    "("~>feats<~")" |
    "()" ^^ {case _ => Set("")}

  def feats:Parser[Set[String]] =
    featName~","~feats ^^ {case f~_~set => set ++ Set(f)} |
    featName ^^ { f => Set(f)}


  def moreProd:Parser[Set[Set[String]]] =
    prod~","~moreProd ^^ {case p~_~set => set + p} |
    prod ^^ {Set(_)}

}
