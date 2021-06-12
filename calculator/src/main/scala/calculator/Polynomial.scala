package calculator

object Polynomial extends PolynomialInterface {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {

    // delta = b ** 2 - 4ac
    new Var(math.pow(b(), 2) - 4 * a() * c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {

    // (-b +- sqrt(delta)) / 2a
    new Var(
      delta() match {
        case d if d < 0 => Set()
        case d => Set(
          (-b() + math.sqrt(d)) / (2 * a()),
          (-b() - math.sqrt(d)) / (2 * a())
        )
      }
    )
  }
}
