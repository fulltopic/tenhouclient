package tenhouclient.impl.mdp

import org.deeplearning4j.rl4j.space.DiscreteSpace
import org.nd4j.linalg.api.ndarray.INDArray
import org.nd4j.linalg.factory.Nd4j

import tenhouclient.impl.ImplConsts._

import scala.util.Random

class TenhouIntegerActionSpace extends DiscreteSpace(0){
  private[this] var random = new Random(47)

  override def randomAction(): Integer = random.nextInt(getSize)

  override def setSeed(seed: Int): Unit = {
    random = new Random(seed)
  }

  override def encode(action: Integer): INDArray = {
    val actVec = Nd4j.zeros(getSize)
    actVec.putScalar(action, 1)

    actVec
  }

  override def getSize(): Int = ActionLenWoAccept

  override def noOp(): Integer = NOOPWoAccept
}
