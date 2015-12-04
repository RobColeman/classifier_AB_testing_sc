package Bayesian

import models.ConfusionMatrix
import breeze.stats.distributions.Dirichlet
import breeze.linalg._

class BayesianConfusionMatrix(confusionMatrix: DenseMatrix[Double],
                              prior: DenseMatrix[Double] = DenseMatrix.ones[Double](2,2)) extends
  ConfusionMatrix(confusionMatrix = confusionMatrix) {

  val dirichlet: Dirichlet[DenseVector[Double], Int] = Dirichlet(prior.toDenseVector)



}
