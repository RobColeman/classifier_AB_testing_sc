package models

import breeze.linalg._
import breeze.stats.distributions.Multinomial

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
    new ConfusionMatrix( old.confusionMatrix + newConfMat)
  }
  def update(old: ConfusionMatrix, scoreAndLabels: Seq[(Double,Double)]): ConfusionMatrix = {
    val newConfusionMatrix: DenseMatrix[Double] = scoreAndLabels.map(mapperPredictionPairsToMatrix).reduce(reduceCMLists)
    new ConfusionMatrix( old.confusionMatrix + newConfusionMatrix)
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



class ConfusionMatrix(val confusionMatrix: DenseMatrix[Double]) {
  require(sum(confusionMatrix) > 0.0, "Confusion matrix must not be empty")

  val n: Double = sum(this.confusionMatrix)
  lazy val confusionMatrixNormalized: DenseMatrix[Double] = this.confusionMatrix.map{ _ / this.n }
  lazy val tn: Double = this.confusionMatrix(0,0)
  lazy val tnRate: Double = this.tn / this.n
  lazy val fn: Double = this.confusionMatrix(0,1)
  lazy val fnRate: Double = this.fn / this.n
  lazy val fp: Double = this.confusionMatrix(1,0)
  lazy val fpRate: Double = this.fp / this.n
  lazy val tp: Double = this.confusionMatrix(1,1)
  lazy val tpRate: Double = this.tp / this.n

  lazy val correct: Double = trace(confusionMatrix)
  lazy val incorrect: Double = trace(confusionMatrix.t)

  // metrics
  lazy val sensitivity: Double = {
    val denom = this.tp + this.fp
    denom > 0.0 match {
      case false => 0.0
      case true => this.tp / denom
    }
  }

  lazy val specificity: Double = {
    val denom =  this.fp + this.tn
    denom > 0.0 match {
      case false => 0.0
      case true => this.tn / denom
    }
  }

  lazy val falsePositiveRate: Double = {
    val denom =  this.fp + this.tn
    denom > 0.0 match {
      case false => 0.0
      case true => this.fp / denom
    }
  }

  lazy val falseNegativeRate: Double = {
    val denom =  this.fn + this.tp
    denom > 0.0 match {
      case false => 0.0
      case true => this.fn / denom
    }
  }

  lazy val precision: Double = {
    val denom = this.tp + this.fp
    denom > 0.0 match {
      case false => 0.0
      case true => tp / denom
    }
  }

  lazy val recall: Double = {
    val denom = this.tp + this.fn
    denom > 0.0 match {
      case false => 0.0
      case true => tp / denom
    }
  }

  lazy val accuracy: Double = trace(confusionMatrix) / sum(confusionMatrix)
  lazy val error: Double = 1.0 - this.accuracy

  lazy val f1Coefficient: Double = {
    val denom = this.precision + this.recall
    denom > 0.0 match {
      case false => 0.0
      case true => (2 * this.precision * this.recall) / (this.precision + this.recall)
    }
  }

  lazy val matthewsCorrelationCoefficient: Double = {
    val num   = (tp * tn) - (fp * fn)
    val denom = Math.sqrt(  (tp + fp) * (tp + fn) * (tn + fp) * (tn + fn)  )
    denom > 0.0 match {
      case false => 0.0
      case true => num / denom
    }
  }


  def +(other: ConfusionMatrix): ConfusionMatrix = ConfusionMatrix( this.confusionMatrix + other.confusionMatrix )
  def -(other: ConfusionMatrix): ConfusionMatrix = ConfusionMatrix( this.confusionMatrix - other.confusionMatrix )
  def update(scoreAndLabels: Seq[(Double,Double)]): ConfusionMatrix = ConfusionMatrix.update(this, scoreAndLabels)
  def update(confMat: DenseMatrix[Double]): ConfusionMatrix = ConfusionMatrix.update(this, confMat)


}


