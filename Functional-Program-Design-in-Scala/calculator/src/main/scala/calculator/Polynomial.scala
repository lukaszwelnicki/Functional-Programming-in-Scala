package calculator

import scala.collection.{immutable, mutable}

object Polynomial extends PolynomialInterface {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    new Signal[Double](b() * b() - 4 * a() * c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    new Signal[Set[Double]]({
      val deltaVal = delta()
      if (deltaVal < 0) Set.empty
      else if (deltaVal == 0) immutable.Set(-b() / (2 * a()))
      else immutable.Set((-b() - Math.sqrt(deltaVal)) / (2 * a()), (-b() + Math.sqrt(deltaVal)) / (2 * a()))
    })
  }
}
