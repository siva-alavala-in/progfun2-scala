package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
                   c: Signal[Double]): Signal[Double] = Signal[Double] {
    val bv = b()
    bv * bv - 4 * a() * c()
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
                       c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = Signal[Set[Double]] {
    val dv = delta()
    val av = a()
    val cv = c()
    val bv = b()
    if (av == 0) {
      Set(-cv / bv)
    } else if (dv < 0) {
      Set.empty
    } else {
      val sd = Math.sqrt(dv)
      Set((-bv - sd) / (av * 2), (-bv + sd) / (av * 2))
    }
  }
}
