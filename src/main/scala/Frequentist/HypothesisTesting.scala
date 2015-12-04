package Frequentist

import models.ConfusionMatrix
import breeze.stats.distributions.Binomial
import org.apache.commons.math3.distribution.BinomialDistribution



object HypothesisTesting {

  private def accuracyCDF(confMatNull: ConfusionMatrix, confMatAlt: ConfusionMatrix): Double = {
    val p0 = confMatNull.accuracy
    val n2 = confMatAlt.n
    val B = new BinomialDistribution(confMatAlt.n.toInt, confMatNull.accuracy)
    B.cumulativeProbability(confMatAlt.correct.toInt)
  }
  def accuracy(confMatNull: ConfusionMatrix, confMatAlt: ConfusionMatrix): Double = {
    1 - accuracyCDF(confMatNull,confMatAlt)
  }
  def accuracy2Way(confMatNull: ConfusionMatrix, confMatAlt: ConfusionMatrix): Double = {
    val F = accuracyCDF(confMatNull,confMatAlt)
    Array(1-F,F).min
  }

}
