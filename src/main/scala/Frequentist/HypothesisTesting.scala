package Frequentist

import helpers.math._
import models.ConfusionMatrix
import org.apache.commons.math3.distribution.BinomialDistribution

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
      2 * squared(confMatAlt.tp) + (confMatAlt.tp * confMatAlt.fn) + (confMatAlt.tp * confMatAlt.fp)
    }.toInt
    val xAlt: Int = 2 * squared(confMatAlt.tp).toInt
    this.binomialCDF(xAlt, nAlt, pNull)
  }

  // chi2 test
  private def mcc(confMatNull: ConfusionMatrix, confMatAlt: ConfusionMatrix): Double = {
    ???
  }



  private def binomialCDF(xAlt: Int, nAlt: Int, pNull: Double): Double = {
    val B = new BinomialDistribution(nAlt, pNull)
    B.cumulativeProbability(xAlt)
  }

}

case class HypothesisTestResult(statistic: String, statisticDelta: Double,
                                cdfX: Double, pValue: Double,
                                rejectNull: Boolean, threshold: Double = 0.05)
