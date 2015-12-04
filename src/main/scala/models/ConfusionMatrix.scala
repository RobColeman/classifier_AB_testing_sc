package models

import breeze.linalg._
import breeze.stats.distributions.Multinomial
import breeze.numerics.round
import scala.util.Random
import scala.math.floor
import scala.collection.mutable

object ConfusionMatrix {
  def apply(confusionMatrix: DenseMatrix[Double]): ConfusionMatrix = new ConfusionMatrix(confusionMatrix)
  def apply(scoreAndLabels: Seq[(Double,Double)]): ConfusionMatrix = {
    val confusionMatrix: DenseMatrix[Double] = scoreAndLabels.map(mapperPredictionPairsToMatrix).reduce(reduceCMLists)
    new ConfusionMatrix(confusionMatrix)
  }
  /* spark
  def apply(scoreAndLabelsRDD: RDD[(Double,Double)]): ConfusionMatrix = {
    val confusionMatrix: DenseMatrix[Double] = scoreAndLabelsRDD.map(mapperPredictionPairsToMatrix).reduce(reduceCMLists)
    new ConfusionMatrix(confusionMatrix)
  }
  */
  def update(old: ConfusionMatrix, newConfMat: DenseMatrix[Double]): ConfusionMatrix = {
    new ConfusionMatrix( old.getConfusionMatrix + newConfMat)
  }
  def update(old: ConfusionMatrix, scoreAndLabels: Seq[(Double,Double)]): ConfusionMatrix = {
    val newConfusionMatrix: DenseMatrix[Double] = scoreAndLabels.map(mapperPredictionPairsToMatrix).reduce(reduceCMLists)
    new ConfusionMatrix( old.getConfusionMatrix + newConfusionMatrix)
  }
  def mapperPredictionPairsToMatrix(t: (Double,Double)): DenseMatrix[Double] = {
    (Math.round(t._1), Math.round(t._2)) match {
      case (0,0) => DenseMatrix((1.0,0.0),(0.0,0.0))
      case (1,0) => DenseMatrix((0.0,1.0),(0.0,0.0))
      case (0,1) => DenseMatrix((0.0,0.0),(1.0,0.0))
      case (1,1) => DenseMatrix((0.0,0.0),(0.0,1.0))
      case _ => DenseMatrix((0.0,0.0),(0.0,0.0))
    }
  }
  def reduceCMLists(left: DenseMatrix[Double], right: DenseMatrix[Double]): DenseMatrix[Double]  = left + right


  def random(n: Int): ConfusionMatrix = {
    var t = n
    val pVec: DenseVector[Double] = DenseVector.rand[Double](4)
    val s: Double = sum(pVec)
    val pVecNorm: DenseVector[Double] = pVec.map{ _ / s }
    val multi = Multinomial(pVecNorm)
    val samples = multi.sample(n)
    val M = new DenseMatrix(2,2,Array(samples.count( _ == 0).toDouble,
                                      samples.count( _ == 1).toDouble,
                                      samples.count( _ == 2).toDouble,
                                      samples.count( _ == 3).toDouble))
    ConfusionMatrix(M)
  }
}



class ConfusionMatrix(private var confusionMatrix: DenseMatrix[Double]) {
  require(sum(confusionMatrix) > 0.0, "Confusion matrix must not be empty")

  def getConfusionMatrix: DenseMatrix[Double] = this.confusionMatrix
  def n: Double = sum(confusionMatrix)
  def tn: Double = confusionMatrix(0,0)
  def fn: Double = confusionMatrix(0,1)
  def fp: Double = confusionMatrix(1,0)
  def tp: Double = confusionMatrix(1,1)

  def correct: Double = trace(confusionMatrix)
  def incorrect: Double = trace(confusionMatrix.t)

  // metrics
  def sensitivity: Double = {
    val denom = this.tp + this.fp
    denom > 0.0 match {
      case false => 0.0
      case true => this.tp / denom
    }
  }

  def specificity: Double = {
    val denom =  this.fp + this.tn
    denom > 0.0 match {
      case false => 0.0
      case true => this.tn / denom
    }
  }

  def falsePositiveRate: Double = {
    val denom =  this.fp + this.tn
    denom > 0.0 match {
      case false => 0.0
      case true => this.fp / denom
    }
  }

  def falseNegativeRate: Double = {
    val denom =  this.fn + this.tp
    denom > 0.0 match {
      case false => 0.0
      case true => this.fn / denom
    }
  }

  def precision: Double = {
    val denom = this.tp + this.fp
    denom > 0.0 match {
      case false => 0.0
      case true => tp / denom
    }
  }

  def recall: Double = {
    val denom = this.tp + this.fn
    denom > 0.0 match {
      case false => 0.0
      case true => tp / denom
    }
  }

  def accuracy: Double = trace(confusionMatrix) / sum(confusionMatrix)
  def error: Double = 1.0 - this.accuracy

  def f1Coefficient: Double = {
    val denom = this.precision + this.recall
    denom > 0.0 match {
      case false => 0.0
      case true => (2 * this.precision * this.recall) / (this.precision + this.recall)
    }
  }

  def matthewsCorrelationCoefficient: Double = {
    val num   = (tp * tn) - (fp * fn)
    val denom = Math.sqrt(  (tp + fp) * (tp + fn) * (tn + fp) * (tn + fn)  )
    denom > 0.0 match {
      case false => 0.0
      case true => num / denom
    }
  }


  def +(other: ConfusionMatrix): ConfusionMatrix = ConfusionMatrix( this.getConfusionMatrix + other.getConfusionMatrix )
  def -(other: ConfusionMatrix): ConfusionMatrix = ConfusionMatrix( this.getConfusionMatrix - other.getConfusionMatrix )
  def update(scoreAndLabels: Seq[(Double,Double)]): ConfusionMatrix = ConfusionMatrix.update(this, scoreAndLabels)
  def update(confMat: DenseMatrix[Double]): ConfusionMatrix = ConfusionMatrix.update(this, confMat)


}


