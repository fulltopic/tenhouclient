package tenhouclient.test

import org.nd4j.linalg.api.ndarray.INDArray
import tenhouclient.config.ClientConfig
import tenhouclient.impl.MessageParseUtilsImpl
import tenhouclient.impl.mdp.{TenhouEncodableMdp, TenhouEncodableMdpFactory}

import scala.util.Random

class RandomNN {
  private val clientConfig =
    new ClientConfig(
          Array[String]("ID0CAF3DF9-HBH66B8c", "testtesttest"),
      "133.242.10.78",
      10080,
      10,
      false
                         )

  private val mdpFactory = new TenhouEncodableMdpFactory(true, clientConfig)
//  private val mdpFactory = new TenhouEncodableMdp(true, -1, clientConfig)
  mdpFactory.newInstance
  private val mdp = mdpFactory.newInstance
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
