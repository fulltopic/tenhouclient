package tenhouclient.impl.mdp

import java.util.concurrent.atomic.AtomicInteger

import akka.event.slf4j.Logger
import org.deeplearning4j.gym.StepReply
import org.deeplearning4j.rl4j.mdp.MDP
import org.deeplearning4j.rl4j.space.ObservationSpace
import tenhouclient.config.ClientConfig

class TenhouEncodableMdpFactory(val workable: Boolean = true, val clientConfig: ClientConfig) extends MDP[TenhouArray, Integer, TenhouIntegerActionSpace] {
  private[this] val logger = Logger("TenhouEncodableMdpFactory")

  override def getObservationSpace: ObservationSpace[TenhouArray] = {
    throw new InvalidMdpOpException("Factory not support observe space operation")
  }

  override def getActionSpace: TenhouIntegerActionSpace = new TenhouIntegerActionSpace()
//  {
//    throw new InvalidMdpOpException("Factory not support action space operation")
//  }

  override def reset: TenhouArray = {
    throw new InvalidMdpOpException("Factory not support reset operation")
  }

  override def close(): Unit = {
    throw new InvalidMdpOpException("Factory not support close operation")
  }

  override def step(action: Integer): StepReply[TenhouArray] = {
    throw new InvalidMdpOpException("Factory not support step operation")
  }

  override def isDone: Boolean = {
    throw new InvalidMdpOpException("Factory not support isDone operation")
  }

  override def newInstance: TenhouEncodableMdp = new TenhouEncodableMdp(true, TenhouEncodableMdpFactory.getNewIndex, clientConfig)

}

object TenhouEncodableMdpFactory {
  private val index = new AtomicInteger(-1)

  def getNewIndex: Int = {
    index.incrementAndGet()
  }
}