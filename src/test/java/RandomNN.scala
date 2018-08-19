import org.nd4j.linalg.api.ndarray.INDArray
import tenhouclient.impl.MessageParseUtilsImpl
import tenhouclient.impl.mdp.TenhouEncodableMdp

import scala.util.Random

class RandomNN {
  private val mdp = new TenhouEncodableMdp(true)
  private val actionRandom = new Random(53)
  private var lastState: INDArray = null

  private def getAction(): Int = {
    val actions = MessageParseUtilsImpl.getAvailableActions(lastState)

    actions(actionRandom.nextInt(actions.length))
  }

  private def callSteps(): Unit = {
    while (!mdp.isDone) {
      val action = getAction()
      val reply = mdp.step(action)
      lastState = reply.getObservation.data
    }
  }

  def steps(): Unit = {
    while (true) {
      lastState = mdp.reset.data
      callSteps()
    }
  }
}
