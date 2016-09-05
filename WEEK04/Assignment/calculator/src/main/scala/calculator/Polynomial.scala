package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double], c: Signal[Double]): Signal[Double] = {
    Signal(Math.pow(b(), 2) - (4 * a() * c()))
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
                       c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    //if (a() == 0) throw new IllegalArgumentException("argument a is zero causing div by zero")
    Signal(
      if (delta() < 0) Set(0)
      else {
        Set(
          ((-b() + Math.sqrt(delta())) / (2 * a())),
          ((-b() - Math.sqrt(delta())) / (2 * a())))
      })
  }
}
