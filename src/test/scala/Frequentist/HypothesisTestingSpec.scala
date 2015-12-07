package Frequentist

import org.scalatest.{PrivateMethodTester, Matchers, WordSpec}
import breeze.linalg._
import models.ConfusionMatrix


class HypothesisTestingSpec extends WordSpec with Matchers with PrivateMethodTester {

  val confMat0 = {
    val m = new DenseMatrix(2, 2, Array(25.0, 25.0, 25.0, 25.0))
    ConfusionMatrix(m)
  }
  val confMat1 = {
    val m = new DenseMatrix(2, 2, Array(28.0, 22.0, 22.0, 28.0))
    ConfusionMatrix(m)
  }
  val confMat2 = {
    val m = new DenseMatrix(2, 2, Array(22.0, 28.0, 28.0, 22.0))
    ConfusionMatrix(m)
  }
  val confMat3 = {
    val m = new DenseMatrix(2, 2, Array(30.0, 20.0, 20.0, 30.0))
    ConfusionMatrix(m)
  }
  val confMat4 = {
    val m = new DenseMatrix(2, 2, Array(20.0, 30.0, 30.0, 20.0))
    ConfusionMatrix(m)
  }

  "HypothesisTests.accuracy" should {
    "fail when it has not diverge right" in {
      HypothesisTesting.accuracy(confMat0, confMat1).statisticDelta > 0 should be(true)
      HypothesisTesting.accuracy(confMat0, confMat1).rejectNull should be(false)
    }
    "fail when it has not diverged left" in {
      HypothesisTesting.accuracy(confMat0, confMat2).statisticDelta < 0 should be(true)
      HypothesisTesting.accuracy(confMat0, confMat2).rejectNull should be(false)
    }
    "should pass when it has diverged right" in {
      HypothesisTesting.accuracy(confMat0, confMat3).statisticDelta > 0 should be(true)
      HypothesisTesting.accuracy(confMat0, confMat3).rejectNull should be(true)
    }
    "fail when it has diverged left" in {
      HypothesisTesting.accuracy(confMat0, confMat4).statisticDelta < 0 should be(true)
      HypothesisTesting.accuracy(confMat0, confMat4).rejectNull should be(false)
    }
  }

  "HypothesisTests.accuracy2way" should {
    "fail when it has not diverge right" in {
      HypothesisTesting.accuracy2Way(confMat0, confMat1).statisticDelta > 0 should be(true)
      HypothesisTesting.accuracy2Way(confMat0, confMat1).rejectNull should be(false)
    }
    "fail when it has not diverge left" in {
      HypothesisTesting.accuracy2Way(confMat0, confMat2).statisticDelta < 0 should be(true)
      HypothesisTesting.accuracy2Way(confMat0, confMat2).rejectNull should be(false)
    }
    "pass when it has diverged right" in {
      HypothesisTesting.accuracy2Way(confMat0, confMat3).statisticDelta > 0 should be(true)
      HypothesisTesting.accuracy2Way(confMat0, confMat3).rejectNull should be(true)
    }
    "pass when it has diverged left" in {
      HypothesisTesting.accuracy2Way(confMat0, confMat4).statisticDelta < 0 should be(true)
      HypothesisTesting.accuracy2Way(confMat0, confMat4).rejectNull should be(true)
    }
  }
  // accuracy cdf

  "HypothesisTests.error" should {
    "fail when it has not diverge left" in {
      HypothesisTesting.error(confMat0, confMat1).statisticDelta < 0 should be(true)
      HypothesisTesting.error(confMat0, confMat1).rejectNull should be(false)
    }
    "fail when it has not diverged right" in {
      HypothesisTesting.error(confMat0, confMat2).statisticDelta > 0 should be(true)
      HypothesisTesting.error(confMat0, confMat2).rejectNull should be(false)
    }
    "pass when it has diverged left" in {
      HypothesisTesting.error(confMat0, confMat3).statisticDelta < 0 should be(true)
      HypothesisTesting.error(confMat0, confMat3).rejectNull should be(true)
    }
    "fail when it has diverged right" in {
      HypothesisTesting.error(confMat0, confMat4).statisticDelta > 0 should be(true)
      HypothesisTesting.error(confMat0, confMat4).rejectNull should be(false)
    }
  }

  "HypothesisTests.error2way" should {
    "fail when it has not diverge right" in {
      HypothesisTesting.error2Way(confMat0,confMat1).statisticDelta < 0 should be (true)
      HypothesisTesting.error2Way(confMat0,confMat1).rejectNull should be (false)
    }
    "fail when it has not diverge left" in {
      HypothesisTesting.error2Way(confMat0,confMat2).statisticDelta > 0 should be (true)
      HypothesisTesting.error2Way(confMat0,confMat2).rejectNull should be (false)
    }
    "pass when it has diverged right" in {
      HypothesisTesting.error2Way(confMat0,confMat3).statisticDelta < 0 should be (true)
      HypothesisTesting.error2Way(confMat0,confMat3).rejectNull should be (true)
    }
    "pass when it has diverged left" in {
      HypothesisTesting.error2Way(confMat0,confMat4).statisticDelta > 0 should be (true)
      HypothesisTesting.error2Way(confMat0,confMat4).rejectNull should be (true)
    }
  }
  // error cdf

  val confMat5 = {
    val m = new DenseMatrix(2, 2, Array(30.0, 20.0, 19.0, 31.0))
    ConfusionMatrix(m)
  }
  val confMat6 = {
    val m = new DenseMatrix(2, 2, Array(20.0, 30.0, 32.0, 18.0))
    ConfusionMatrix(m)
  }
  val confMat7 = {
    val m = new DenseMatrix(2, 2, Array(20.0, 32.0, 30.0, 18.0))
    ConfusionMatrix(m)
  }

  "HypothesisTests.recall" should {
    "fail when it has not diverge right" in {
      HypothesisTesting.recall(confMat0, confMat1).statisticDelta > 0 should be(true)
      HypothesisTesting.recall(confMat0, confMat1).rejectNull should be(false)
    }
    "fail when it has not diverged left" in {
      HypothesisTesting.recall(confMat0, confMat2).statisticDelta < 0 should be(true)
      HypothesisTesting.recall(confMat0, confMat2).rejectNull should be(false)
    }
    "pass when it has diverged right" in {
      HypothesisTesting.recall(confMat0, confMat5).statisticDelta > 0 should be(true)
      HypothesisTesting.recall(confMat0, confMat5).rejectNull should be(true)
    }
    "fail when it has diverged left" in {
      HypothesisTesting.recall(confMat0, confMat6).statisticDelta < 0 should be(true)
      HypothesisTesting.recall(confMat0, confMat6).rejectNull should be(false)
    }
  }

  "HypothesisTests.recall2Way" should {
    "fail when it has not diverge right" in {
      HypothesisTesting.recall2Way(confMat0, confMat1).statisticDelta > 0 should be(true)
      HypothesisTesting.recall2Way(confMat0, confMat1).rejectNull should be(false)
    }
    "fail when it has not diverged left" in {
      HypothesisTesting.recall2Way(confMat0, confMat2).statisticDelta < 0 should be(true)
      HypothesisTesting.recall2Way(confMat0, confMat2).rejectNull should be(false)
    }
    "pass when it has diverged right" in {
      HypothesisTesting.recall2Way(confMat0, confMat5).statisticDelta > 0 should be(true)
      HypothesisTesting.recall2Way(confMat0, confMat5).rejectNull should be(true)
    }
    "pass when it has diverged left" in {
      HypothesisTesting.recall2Way(confMat0, confMat6).statisticDelta < 0 should be(true)
      HypothesisTesting.recall2Way(confMat0, confMat6).rejectNull should be(true)
    }
  }

  "HypothesisTests.precision" should {
    "fail when it has not diverge right" in {
      HypothesisTesting.precision(confMat0, confMat1).statisticDelta > 0 should be(true)
      HypothesisTesting.precision(confMat0, confMat1).rejectNull should be(false)
    }
    "fail when it has not diverged left" in {
      HypothesisTesting.precision(confMat0, confMat2).statisticDelta < 0 should be(true)
      HypothesisTesting.precision(confMat0, confMat2).rejectNull should be(false)
    }
    "pass when it has diverged right" in {
      HypothesisTesting.precision(confMat0, confMat5).statisticDelta > 0 should be(true)
      HypothesisTesting.precision(confMat0, confMat5).rejectNull should be(true)
    }
    "fail when it has diverged left" in {
      HypothesisTesting.precision(confMat0, confMat7).statisticDelta < 0 should be(true)
      HypothesisTesting.precision(confMat0, confMat7).rejectNull should be(false)
    }
  }

  "HypothesisTests.precision2Way" should {
    "fail when it has not diverge right" in {
      HypothesisTesting.precision2Way(confMat0, confMat1).statisticDelta > 0 should be(true)
      HypothesisTesting.precision2Way(confMat0, confMat1).rejectNull should be(false)
    }
    "fail when it has not diverged left" in {
      HypothesisTesting.precision2Way(confMat0, confMat2).statisticDelta < 0 should be(true)
      HypothesisTesting.precision2Way(confMat0, confMat2).rejectNull should be(false)
    }
    "pass when it has diverged right" in {
      HypothesisTesting.precision2Way(confMat0, confMat5).statisticDelta > 0 should be(true)
      HypothesisTesting.precision2Way(confMat0, confMat5).rejectNull should be(true)
    }
    "fail when it has diverged left" in {
      HypothesisTesting.precision2Way(confMat0, confMat7).statisticDelta < 0 should be(true)
      HypothesisTesting.precision2Way(confMat0, confMat7).rejectNull should be(true)
    }
  }


  val confMat8 = {
    val m = new DenseMatrix(2, 2, Array(25.0, 21.0, 29.0, 25.0))
    ConfusionMatrix(m)
  }
  val confMat9 = {
    val m = new DenseMatrix(2, 2, Array(24.0, 21.0, 29.0, 26.0))
    ConfusionMatrix(m)
  }
  val confMat10 = {
    val m = new DenseMatrix(2, 2, Array(26.0, 21.0, 29.0, 24.0))
    ConfusionMatrix(m)
  }
  val confMat11 = {
    val m = new DenseMatrix(2, 2, Array(28.0, 21.0, 29.0, 22.0))
    ConfusionMatrix(m)
  }

  "HypothesisTests.f1" should {
    "fail when it has not divereg at all" in {
      HypothesisTesting.f1(confMat0, confMat8).statisticDelta should be(0.0)
      HypothesisTesting.f1(confMat0, confMat8).rejectNull should be(false)
    }
    "fail when it has not diverge right" in {
      HypothesisTesting.f1(confMat0, confMat9).statisticDelta > 0 should be(true)
      HypothesisTesting.f1(confMat0, confMat9).rejectNull should be(false)
    }
    "fail when it has not diverged left" in {
      HypothesisTesting.f1(confMat0, confMat10).statisticDelta < 0 should be(true)
      HypothesisTesting.f1(confMat0, confMat10).rejectNull should be(false)
    }
    "pass when it has diverged right" in {
      HypothesisTesting.f1(confMat0, confMat5).statisticDelta > 0 should be(true)
      HypothesisTesting.f1(confMat0, confMat5).rejectNull should be(true)
    }
    "fail when it has diverged left" in {
      HypothesisTesting.f1(confMat0, confMat7).statisticDelta < 0 should be(true)
      HypothesisTesting.f1(confMat0, confMat7).rejectNull should be(false)
    }
  }


  "HypothesisTests.f12Way" should {
    "fail when it has not divereg at all" in {
      HypothesisTesting.f12Way(confMat0, confMat8).statisticDelta should be(0.0)
      HypothesisTesting.f12Way(confMat0, confMat8).rejectNull should be(false)
    }
    "fail when it has not diverge right" in {
      HypothesisTesting.f12Way(confMat0, confMat9).statisticDelta > 0 should be(true)
      HypothesisTesting.f12Way(confMat0, confMat9).rejectNull should be(false)
    }
    "fail when it has not diverged left" in {
      HypothesisTesting.f12Way(confMat0, confMat10).statisticDelta < 0 should be(true)
      HypothesisTesting.f12Way(confMat0, confMat10).rejectNull should be(false)
    }
    "pass when it has diverged right" in {
      HypothesisTesting.f12Way(confMat0, confMat5).statisticDelta > 0 should be(true)
      HypothesisTesting.f12Way(confMat0, confMat5).rejectNull should be(true)
    }
    "pass when it has diverged left" in {
      HypothesisTesting.f12Way(confMat0, confMat7).statisticDelta < 0 should be(true)
      HypothesisTesting.f12Way(confMat0, confMat7).rejectNull should be(true)
    }
  }

}
