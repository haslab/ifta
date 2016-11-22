package ifta

import java.io.{FileWriter, BufferedWriter}

import ifta.analyse.IFTA2FTA
import ifta.analyse.Simplify
import ifta.backend.{Springy,Vis}

object DSL {
  // to help building clock constraints
  class CVar(n:String) {
    def <(f:Int): ClockCons = LT(n,f)
    def >(f:Int): ClockCons = GT(n,f)
    def <=(f:Int): ClockCons = LE(n,f)
    def >=(f:Int): ClockCons = GE(n,f)
  }

  class ELoc(i:Int) {
    def -->(other:Int): Edge = Edge(i,true,Set(),Set(),true,other)
  }

//  implicit def toNIFTA(iFTA:IFTA): NIFTA = NIFTA(Set(iFTA))
  implicit def toFeat(s:String): Feat = Feat(s)
  implicit def toCVar(s:String): CVar = new CVar(s)
  implicit def boolToCC(b:Boolean): ClockCons =
    if (b) CTrue else throw new RuntimeException("clock constraints cannot be false")
  implicit def boolToFE(b:Boolean): FExp =
    if (b) FTrue else FNot(FTrue)
  implicit def intToELoc(i:Int): ELoc = new ELoc(i)

  def not(fExp: FExp) = FNot(fExp)
  val newifta = IFTA(Set(0),0,Set(),Set(),Set(),Set(),Map(),true,Set(),Set(),Map())
//  val nifta = NIFTA(Set())

  val toDot = ifta.backend.Dot
  def con2dot(nIFTA: NIFTA) = ifta.backend.Dot.connector(nIFTA)

  def toSpringy(iFTA: IFTA) = Springy(iFTA)
  def toSpringy(iFTA: IFTA,file:String) = {
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(Springy(iFTA))
    bw.close()
  }

  def con2Springy(nIFTA: NIFTA) = Springy.connector(nIFTA)
  def con2Springy(nIFTA: NIFTA,file:String) = {
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(Springy.connector(nIFTA))
    bw.close()
  }

  def con2Vis(nIFTA: NIFTA) = Vis.connector(nIFTA)
  def con2Vis(nIFTA: NIFTA,file:String) = {
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(Vis.connector(nIFTA))
    bw.close()
  }

  def toFTA = IFTA2FTA

  // NIFTA -> NFTA -> UPPAAL
  def toUppaal(ifta:IFTA) = backend.Uppaal(NFTA(Set(IFTA2FTA.flatten(ifta))))
  def toUppaal(ifta:IFTA,file:String) = {
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(backend.Uppaal(NFTA(Set(IFTA2FTA.flatten(ifta))))) // IFTA -> FTA -> UPPAAL
    bw.close()
  }
  def toUppaal(nIFTA: NIFTA) = backend.Uppaal(IFTA2FTA(nIFTA))
  def toUppaal(nIFTA: NIFTA, file:String) = {
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(backend.Uppaal(IFTA2FTA(nIFTA))) // NIFTA -> NFTA -> UPPAAL
    bw.close()
  }
}
