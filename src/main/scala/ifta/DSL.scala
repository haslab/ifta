package ifta

import java.io.{FileWriter, BufferedWriter}

object DSL {
  // to help building clock constraints
  class CVar(n:String) {
    def <(f:Float): ClockCons = LT(n,f)
    def >(f:Float): ClockCons = GT(n,f)
    def <=(f:Float): ClockCons = LE(n,f)
    def >=(f:Float): ClockCons = GE(n,f)
  }

  class ELoc(i:Int) {
    def -->(other:Int): Edge = Edge(i,true,Set(),Set(),true,other)
  }

  implicit def toFeat(s:String): Feat = Feat(s)
  implicit def toCVar(s:String): CVar = new CVar(s)
  implicit def boolToCC(b:Boolean): ClockCons =
    if (b) CTrue else throw new RuntimeException("clock constraints cannot be false")
  implicit def boolToFE(b:Boolean): FExp =
    if (b) FTrue else FNot(FTrue)
  implicit def intToELoc(i:Int): ELoc = new ELoc(i)

  def not(fExp: FExp) = FNot(fExp)
  val ifta = IFTA(Set(),Set(),Set(),Set(),Set(),Set(),Map(),true,Set(),Set(),Set())

  def toDot(iFTA: IFTA) = backend.Dot(iFTA)
  def toUpp(iFTA: IFTA) = backend.Uppaal(iFTA)
  def toUpp(iFTA: IFTA,file:String) = {
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(backend.Uppaal(iFTA))
    bw.close()
  }

}
