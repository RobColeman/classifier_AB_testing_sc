package Frequentist

import helpers.math._
import scala.math
import models.ConfusionMatrix
import org.apache.commons.math3.distribution.BinomialDistribution
import org.apache.commons.math3.distribution.ChiSquaredDistribution



object HypothesisTesting {

  // Accuracy and Error
  def accuracy(confMatNull: ConfusionMatrix, confMatAlt: ConfusionMatrix, threshold: Double = 0.05): HypothesisTestResult = {
    val F = this.accuracyCDF(confMatNull, confMatAlt)
    val pValue = 1 - F
    val passed = pValue < threshold

    HypothesisTestResult(
      statistic = "accuracy",
      statisticDelta = confMatAlt.accuracy - confMatNull.accuracy,
      cdfX = F,
      pValue = pValue,
      rejectNull = passed,
      threshold = threshold
    )
  }

  def accuracy2Way(confMatNull: ConfusionMatrix, confMatAlt: ConfusionMatrix, threshold: Double = 0.05): HypothesisTestResult = {
    val F = this.accuracyCDF(confMatNull, confMatAlt)
    val pValue = Array(1-F,F).min
    val passed = pValue < threshold

    HypothesisTestResult(
      statistic = "accuracy",
      statisticDelta = confMatAlt.accuracy - confMatNull.accuracy,
      cdfX = F,
      pValue = pValue,
      rejectNull = passed,
      threshold = threshold
    )
  }

  private def accuracyCDF(confMatNull: ConfusionMatrix, confMatAlt: ConfusionMatrix): Double = {
    val pNull = confMatNull.accuracy
    val nAlt = confMatAlt.n.toInt
    val xAlt = confMatAlt.correct.toInt
    this.binomialCDF(xAlt, nAlt, pNull)
  }

  // error should test left (less error is improvement)
  def error(confMatNull: ConfusionMatrix, confMatAlt: ConfusionMatrix, threshold: Double = 0.05): HypothesisTestResult = {
    val F = this.errorCDF(confMatNull, confMatAlt)
    val pValue = F
    val passed = pValue < threshold

    HypothesisTestResult(
      statistic = "error",
      statisticDelta = confMatAlt.error - confMatNull.error,
      cdfX = F,
      pValue = pValue,
      rejectNull = passed,
      threshold = threshold
    )
  }

  def error2Way(confMatNull: ConfusionMatrix, confMatAlt: ConfusionMatrix, threshold: Double = 0.05): HypothesisTestResult = {
    val F = this.errorCDF(confMatNull, confMatAlt)
    val pValue = Array(1-F,F).min
    val passed = pValue < threshold

    HypothesisTestResult(
      statistic = "error",
      statisticDelta = confMatAlt.error - confMatNull.error,
      cdfX = F,
      pValue = pValue,
      rejectNull = passed,
      threshold = threshold
    )
  }

  private def errorCDF(confMatNull: ConfusionMatrix, confMatAlt: ConfusionMatrix): Double = {
    val pNull = confMatNull.error
    val nAlt = confMatAlt.n.toInt
    val xAlt = confMatAlt.incorrect.toInt
    this.binomialCDF(xAlt, nAlt, pNull)
  }

  // precision and recall
  def recall(confMatNull: ConfusionMatrix, confMatAlt: ConfusionMatrix, threshold: Double = 0.05): HypothesisTestResult = {
    val F = this.recallCDF(confMatNull, confMatAlt)
    val pValue = 1 - F
    val passed = pValue < threshold

    HypothesisTestResult(
      statistic = "recall",
      statisticDelta = confMatAlt.recall - confMatNull.recall,
      cdfX = F,
      pValue = pValue,
      rejectNull = passed,
      threshold = threshold
    )
  }

  def recall2Way(confMatNull: ConfusionMatrix, confMatAlt: ConfusionMatrix, threshold: Double = 0.05): HypothesisTestResult = {
    val F = this.recallCDF(confMatNull, confMatAlt)
    val pValue = Array(1-F,F).min
    val passed = pValue < threshold

    HypothesisTestResult(
      statistic = "recall",
      statisticDelta = confMatAlt.recall - confMatNull.recall,
      cdfX = F,
      pValue = pValue,
      rejectNull = passed,
      threshold = threshold
    )
  }

  private def recallCDF(confMatNull: ConfusionMatrix, confMatAlt: ConfusionMatrix): Double = {
    val pNull = confMatNull.recall
    val nAlt = (confMatAlt.tp + confMatAlt.fn).toInt
    val xAlt = confMatAlt.tp.toInt
    this.binomialCDF(xAlt, nAlt, pNull)
  }


  def precision(confMatNull: ConfusionMatrix, confMatAlt: ConfusionMatrix, threshold: Double = 0.05): HypothesisTestResult = {
    val F = this.precisionCDF(confMatNull, confMatAlt)
    val pValue = 1 - F
    val passed = pValue < threshold

    HypothesisTestResult(
      statistic = "precision",
      statisticDelta = confMatAlt.precision - confMatNull.precision,
      cdfX = F,
      pValue = pValue,
      rejectNull = passed,
      threshold = threshold
    )
  }

  def precision2Way(confMatNull: ConfusionMatrix, confMatAlt: ConfusionMatrix, threshold: Double = 0.05): HypothesisTestResult = {
    val F = this.precisionCDF(confMatNull, confMatAlt)
    val pValue = Array(1-F,F).min
    val passed = pValue < threshold

    HypothesisTestResult(
      statistic = "precision",
      statisticDelta = confMatAlt.precision - confMatNull.precision,
      cdfX = F,
      pValue = pValue,
      rejectNull = passed,
      threshold = threshold
    )
  }

  private def precisionCDF(confMatNull: ConfusionMatrix, confMatAlt: ConfusionMatrix): Double = {
    val pNull = confMatNull.precision
    val nAlt = (confMatAlt.tp + confMatAlt.fp).toInt
    val xAlt = confMatAlt.tp.toInt
    this.binomialCDF(xAlt, nAlt, pNull)
  }




  // f-1 ~ Binomial
  def f1(confMatNull: ConfusionMatrix, confMatAlt: ConfusionMatrix, threshold: Double = 0.05): HypothesisTestResult = {
    val F = this.f1CDF(confMatNull, confMatAlt)
    val pValue = 1 - F
    val passed = pValue < threshold

    HypothesisTestResult(
      statistic = "f1",
      statisticDelta = confMatAlt.f1Coefficient - confMatNull.f1Coefficient,
      cdfX = F,
      pValue = pValue,
      rejectNull = passed,
      threshold = threshold
    )
  }

  def f12Way(confMatNull: ConfusionMatrix, confMatAlt: ConfusionMatrix, threshold: Double = 0.05): HypothesisTestResult = {
    val F = this.f1CDF(confMatNull, confMatAlt)
    val pValue = Array(1-F,F).min
    val passed = pValue < threshold

    HypothesisTestResult(
      statistic = "f1",
      statisticDelta = confMatAlt.f1Coefficient - confMatNull.f1Coefficient,
      cdfX = F,
      pValue = pValue,
      rejectNull = passed,
      threshold = threshold
    )
  }

  private def f1CDF(confMatNull: ConfusionMatrix, confMatAlt: ConfusionMatrix): Double = {
    val pNull = confMatNull.f1Coefficient
    val nAlt: Int = {
      val n = 2 * squared(confMatAlt.tp).toInt + (confMatAlt.tp * confMatAlt.fn) + (confMatAlt.tp * confMatAlt.fp) + (confMatAlt.fn * confMatAlt.fp)
      n.toInt
    }
    val xAlt: Int = 2 * squared(confMatAlt.tp).toInt
    this.binomialCDF(xAlt, nAlt, pNull)
  }

  def mccH0Uniform(confMat: ConfusionMatrix, threshold: Double = 0.05): HypothesisTestResult = {
    // H0: uniform distribution over the confusion matrix
    val x = confMat.n * squared(confMat.matthewsCorrelationCoefficient)
    val F = chi2CDF(x, confMat.n.toInt)
    val pValue = 1.0 - F
    val passed = pValue < threshold

    HypothesisTestResult(
      statistic = "mcc",
      statisticDelta = math.abs(confMat.matthewsCorrelationCoefficient),
      cdfX = F,
      pValue = pValue,
      rejectNull = passed,
      threshold = threshold
    )
  }

  // n * squared(MCC) ~ Chi2, where n = confmat.n
  def mcc(confMatNull: ConfusionMatrix, confMatAlt: ConfusionMatrix, threshold: Double = 0.05): HypothesisTestResult = {
    val hNull = confMatNull.confusionMatrixNormalized
    val hAlt = confMatAlt.confusionMatrixNormalized
    val n: Int = confMatAlt.n.toInt
    val x: Double = n * hNull.toArray.zip(hAlt.toArray).map{ case (pNull,pAlt) =>
      pNull * squared( (pAlt - pNull)  / pNull )
    }.sum

    val F = chi2CDF(x, n - 1)
    val pValue = 1.0 - F
    val passed = pValue < threshold

    HypothesisTestResult(
      statistic = "mcc",
      statisticDelta = confMatAlt.matthewsCorrelationCoefficient - confMatNull.matthewsCorrelationCoefficient,
      cdfX = F,
      pValue = pValue,
      rejectNull = passed,
      threshold = threshold
    )
  }

  private def binomialCDF(xAlt: Int, nAlt: Int, pNull: Double): Double = {
    val B = new BinomialDistribution(nAlt, pNull)
    B.cumulativeProbability(xAlt)
  }

  private def chi2CDF(x: Double, k: Int): Double = {
    val ch2: ChiSquaredDistribution = new ChiSquaredDistribution(k)
    ch2.cumulativeProbability(x)
  }

}

case class HypothesisTestResult(statistic: String, statisticDelta: Double,
                                cdfX: Double, pValue: Double,
                                rejectNull: Boolean, threshold: Double = 0.05)
