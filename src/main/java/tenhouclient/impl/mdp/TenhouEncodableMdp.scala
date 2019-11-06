package tenhouclient.impl.mdp

import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicInteger

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.event.slf4j.Logger
import akka.pattern.Patterns
import akka.util.Timeout
import org.deeplearning4j.gym.StepReply
import org.deeplearning4j.rl4j.mdp.MDP
import org.deeplearning4j.rl4j.space.ObservationSpace
import org.json.JSONObject
import tenhouclient.utils.MessageParseUtils.{ResetAction, StartConnection}
import tenhouclient.impl.ImplConsts._
import tenhouclient.client.TenhouClient
import tenhouclient.config.ClientConfig
import tenhouclient.impl.MsgHelpImpl
import tenhouclient.conn.msgs.{ActionRequest, ActionResponse}

import scala.concurrent.{Await, Future}

//class TenhouEncodableMdp(val workable: Boolean = true, val index: Int = 0) extends MDP[TenhouArray, Integer, TenhouIntegerActionSpace]{
class TenhouEncodableMdp(val workable: Boolean = true, val index: Int, val clientConfig: ClientConfig) extends MDP[TenhouArray, Integer, TenhouIntegerActionSpace]{

  private[this] val logger = Logger("TenhouEncodableMdp")

  private[this] var done = false
  var lastStep: Int = 1
  var isReach: Boolean = false
  var lastTile: Int = InvalidTile
  var lastActions: JSONObject = null
  var isTourEnd: Boolean = true
  var isDoneFlag: Boolean = false

  private var client: ActorRef = null

  if (workable) {
    val system = ActorSystem("testSystem")
//    val props = Props(new TenhouClient(index, new MsgHelpImpl()))
    val props = Props(new TenhouClient(index, clientConfig, new MsgHelpImpl()))
//    client = system.actorOf(props, "tenhouClient" + index)
    client = system.actorOf(props, "tenhouClient" + index)
    logger.info("Tenhou mdp created")
  }


  override def getObservationSpace: ObservationSpace[TenhouArray] = new TenhouObservationSpace[TenhouArray]()

  override def getActionSpace: TenhouIntegerActionSpace = new TenhouIntegerActionSpace()

  val timeout = new Timeout(100000L, TimeUnit.SECONDS) // As infinity

  def initClient(): Unit = {
    if (isTourEnd) {
      logger.info("Try initClient")
      val rspFuture: Future[AnyRef] = Patterns.ask(client, StartConnection, timeout)
      val rspObj = Await.result(rspFuture, timeout.duration).asInstanceOf[String]
      logger.info("-----------------> End of init")
    }
  }


  override def reset: TenhouArray = {
    logger.info("in reset")
    initClient()

    val request = generateRequest(ResetAction, lastTile)
    val rspFuture: Future[AnyRef] = Patterns.ask(client, request, timeout)
    val rspObj = Await.result(rspFuture, timeout.duration).asInstanceOf[ActionResponse]
    isReach = rspObj.isReach
    isDoneFlag = rspObj.reply.isDone
    lastTile = rspObj.tile
    lastActions = rspObj.reply.getInfo
    isTourEnd = rspObj.isTourEnd


    new TenhouArray(rspObj.reply.getObservation)
  }

  override def close(): Unit = {
    //TODO: To send close to connection
  }


  override def step(action: Integer): StepReply[TenhouArray] = {
    val request = generateRequest(action, lastTile)
    val rspFuture: Future[AnyRef] = Patterns.ask(client, request, timeout)
    val rspObj = Await.result(rspFuture, timeout.duration).asInstanceOf[ActionResponse]
    isReach = rspObj.isReach
    isDoneFlag = rspObj.reply.isDone
    lastTile = rspObj.tile
    lastActions = rspObj.reply.getInfo
    isTourEnd = rspObj.isTourEnd

    //TODO: some preprocess of input? or processed in nn?
    new StepReply(new TenhouArray(rspObj.reply.getObservation), rspObj.reply.getReward, rspObj.reply.isDone, null)
  }

  private[this] def generateRequest(action: Int, tile: Int): ActionRequest = {
    new ActionRequest(action, tile, isReach)
  }

  override def isDone: Boolean = isDoneFlag

//  override def newInstance: TenhouEncodableMdp = new TenhouEncodableMdp(true, TenhouEncodableMdp.getNewIndex(), clientConfig)
  override def newInstance: TenhouEncodableMdp = {
    throw new InvalidMdpOpException("TenhouEncodableMdp not support factory function")
  }

}

//object TenhouEncodableMdp {
//  private val index = new AtomicInteger(-1)
//
//  def getNewIndex(): Int = {
//    index.incrementAndGet()
//  }
//}