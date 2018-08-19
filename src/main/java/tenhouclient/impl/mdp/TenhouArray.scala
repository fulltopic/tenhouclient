package tenhouclient.impl.mdp

import org.deeplearning4j.rl4j.space.Encodable
import org.nd4j.linalg.api.ndarray.INDArray
import tenhouclient.impl.ImplConsts.PeerStateLen

class TenhouArray(val data: INDArray) extends Encodable {
  override def toArray: Array[Double] = {
    val array = Array.fill[Double](PeerStateLen)(0)
    for (i <- array.indices) {
      array(i) = data.getDouble(i)
    }

    array
  }

  override def toString: String = {
    val a = toArray
    a.mkString(", ")
  }
}

