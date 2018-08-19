package tenhouclient.impl.mdp

import org.deeplearning4j.rl4j.space.ObservationSpace
import org.nd4j.linalg.api.ndarray.INDArray
import org.nd4j.linalg.factory.Nd4j

import tenhouclient.impl.ImplConsts._

class TenhouObservationSpace[O] extends ObservationSpace[O] {
  override def getName(): String = "TenhouObservationSpace"

  override def getShape(): Array[Int] = Array[Int](PeerStateLen)

  override def getLow(): INDArray = Nd4j.zeros(1, PeerStateLen)

  override def getHigh: INDArray = Nd4j.ones(1, PeerStateLen).muli(Int.MaxValue)
}
