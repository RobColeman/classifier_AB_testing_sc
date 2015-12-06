package Frequentist

import org.scalatest.{PrivateMethodTester, Matchers, WordSpec}
import breeze.linalg._
import models.ConfusionMatrix


class HypothesisTestingSpec extends WordSpec with Matchers with PrivateMethodTester {



  "HypothesisTests - Error and Accuracy" should {
    val confMat0 = {
      val cm = new DenseMatrix(2, 2, Array(25.0, 25.0, 25.0, 25.0))
      ConfusionMatrix(cm)
    }
    val confMat1 = {
      val cm = new DenseMatrix(2, 2, Array(28.0, 22.0, 22.0, 28.0))
      ConfusionMatrix(cm)
    }
    val confMat2 = {
      val cm = new DenseMatrix(2, 2, Array(22.0, 28.0, 28.0, 22.0))
      ConfusionMatrix(cm)
    }
    val confMat3 = {
      val cm = new DenseMatrix(2, 2, Array(30.0, 20.0, 20.0, 30.0))
      ConfusionMatrix(cm)
    }
    val confMat4 = {
      val cm = new DenseMatrix(2, 2, Array(20.0, 30.0, 30.0, 20.0))
      ConfusionMatrix(cm)
    }


    // accuracy
    "accuracy should fail when it has not diverge right" in {
      HypothesisTesting.accuracy(confMat0, confMat1).statisticDelta > 0 should be (true)
      HypothesisTesting.accuracy(confMat0, confMat1).rejectNull should be (false)
    }
    "accuracy should fail when it has not diverged left" in {
      HypothesisTesting.accuracy(confMat0, confMat2).statisticDelta < 0 should be (true)
      HypothesisTesting.accuracy(confMat0, confMat2).rejectNull should be (false)
    }
    "accuracy should fail when it has has diverged right" in {
      HypothesisTesting.accuracy(confMat0, confMat3).statisticDelta < 0 should be (true)
      HypothesisTesting.accuracy(confMat0, confMat3).rejectNull should be (false)
    }
    "accuracy should pass when it has diverged left" in {
      HypothesisTesting.accuracy(confMat0, confMat4).statisticDelta > 0 should be (true)
      HypothesisTesting.accuracy(confMat0, confMat4).rejectNull should be (true)
    }

    "accuracy2way should fail when it has not diverge right" in {
      HypothesisTesting.accuracy2Way(confMat0, confMat1).statisticDelta > 0 should be (true)
      HypothesisTesting.accuracy2Way(confMat0, confMat1).rejectNull should be (false)
    }
    "accuracy2way should fail when it has not diverge left" in {
      HypothesisTesting.accuracy2Way(confMat0, confMat2).statisticDelta < 0 should be (true)
      HypothesisTesting.accuracy2Way(confMat0, confMat2).rejectNull should be (false)
    }
    "accuracy2way should pass when it has diverged right" in {
      HypothesisTesting.accuracy2Way(confMat0,confMat3).statisticDelta > 0 should be (true)
      HypothesisTesting.accuracy2Way(confMat0,confMat3).rejectNull should be (true)
    }
    "accuracy2way should pass when it has diverged left" in {
      HypothesisTesting.accuracy2Way(confMat0,confMat4).statisticDelta < 0 should be (true)
      HypothesisTesting.accuracy2Way(confMat0,confMat4).rejectNull should be (true)
    }

    // Error
    "error should fail when it has not diverge right" in {
      HypothesisTesting.error(confMat0,confMat1).statisticDelta > 0 should be (true)
      HypothesisTesting.error(confMat0,confMat1).rejectNull should be (false)
    }

    "error should pass when it has not diverged left" in {
      HypothesisTesting.error(confMat0,confMat2).statisticDelta > 0 should be (true)
      HypothesisTesting.error(confMat0,confMat2).rejectNull should be (true)
    }

    "error should pass when it has diverged left" in {
      HypothesisTesting.error(confMat0,confMat3).statisticDelta < 0 should be (true)
      HypothesisTesting.error(confMat0,confMat3).rejectNull should be (false)
    }

    "error should fail when it has not diverge right"in {
      HypothesisTesting.error(confMat0,confMat1).statisticDelta > 0 should be (true)
      HypothesisTesting.error(confMat0,confMat1).rejectNull should be (false)
    }
    "error should fail when it has not diverged right" in {
      HypothesisTesting.error(confMat0,confMat2).statisticDelta < 0 should be (true)
      HypothesisTesting.error(confMat0,confMat2).rejectNull should be (false)
    }
    "error should pass when it has diverged right" in {
      HypothesisTesting.error(confMat0,confMat4).statisticDelta < 0 should be (true)
      HypothesisTesting.error(confMat0,confMat4).rejectNull should be (true)
    }

    "error2way should fail when it has not diverge right" in {
      HypothesisTesting.error2Way(confMat0,confMat1).statisticDelta > 0 should be (true)
      HypothesisTesting.error2Way(confMat0,confMat1).rejectNull should be (false)
    }
    "error2way should fail when it has not diverge left" in {
      HypothesisTesting.error2Way(confMat0,confMat2).statisticDelta < 0 should be (true)
      HypothesisTesting.error2Way(confMat0,confMat2).rejectNull should be (false)
    }
    "error2way should pass when it has diverged right" in {
      HypothesisTesting.error2Way(confMat0,confMat3).statisticDelta > 0 should be (true)
      HypothesisTesting.error2Way(confMat0,confMat3).rejectNull should be (true)
    }
    "error2way should pass when it has diverged right" in {
      HypothesisTesting.error2Way(confMat0,confMat4).statisticDelta < 0 should be (true)
      HypothesisTesting.error2Way(confMat0,confMat4).rejectNull should be (true)
    }
  }

  "Hypothesis testing - precision adn recall" should {
    val confMat0 = {
      val cm = new DenseMatrix(2, 2, Array(25.0, 25.0, 25.0, 25.0))
      ConfusionMatrix(cm)
    }
    val confMat1 = {
      val cm = new DenseMatrix(2, 2, Array(28.0, 22.0, 22.0, 28.0))
      ConfusionMatrix(cm)
    }
    val confMat2 = {
      val cm = new DenseMatrix(2, 2, Array(22.0, 28.0, 28.0, 22.0))
      ConfusionMatrix(cm)
    }
    val confMat3 = {
      val cm = new DenseMatrix(2, 2, Array(30.0, 20.0, 20.0, 30.0))
      ConfusionMatrix(cm)
    }
    val confMat4 = {
      val cm = new DenseMatrix(2, 2, Array(20.0, 30.0, 30.0, 20.0))
      ConfusionMatrix(cm)
    }


  }


}
