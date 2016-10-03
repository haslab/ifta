package ifta

object DSL {
  class CVar(n:String) {
    def <(f:Float): ClockCons = LT(n,f)
    def >(f:Float): ClockCons = GT(n,f)
    def <=(f:Float): ClockCons = LE(n,f)
    def >=(f:Float): ClockCons = GE(n,f)
  }
  implicit def toFeat(s:String): Feat = Feat(s)
  implicit def toCVar(s:String): CVar = new CVar(s)
  implicit def boolToCC(b:Boolean): ClockCons =
    if (b) CTrue else throw new RuntimeException("clock constraints cannot be false")
  implicit def boolToFE(b:Boolean): FExp =
    if (b) FTrue else FNot(FTrue)

  def not(fExp: FExp) = FNot(fExp)

  def toDot(iFTA: IFTA) = backend.Dot(iFTA)

}
