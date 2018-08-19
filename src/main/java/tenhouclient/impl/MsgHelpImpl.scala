package tenhouclient.impl

import akka.event.slf4j.Logger
import tenhouclient.conn.msgs.ActionResponse
import tenhouclient.utils.TenhouConsts.TileNum
import tenhouclient.utils.MessageParseUtils
import tenhouclient.client.MsgHelper

class MsgHelpImpl extends MsgHelper{
  private val logger = Logger("MsgHelper")
  private val state = new MdpInnerState()
  private val actionHelper = new ActionParser(state)
  private val msgHelper = new MessageParser(state)
  private var oya: Int = 0 //don't what to do


  def iam(): Int = {
    state.iam
  }

  def isInReachAction(): Boolean = state.inReachAction

  //  private def turnOnReachAction(): Unit = {
  //    state.inReachAction = true
  //  }
  private def turnOffReachAction(): Unit = {
    state.inReachAction = false
    state.decideReach = false
    actionHelper.clearTry()
  }

  def reached(): Boolean = {
    MessageParseUtilsImpl.reached(state)
  }

  def getKAMsg(): String = {
    MessageParseUtils.getKAMsg()
  }

  def getHeloMsg(userName: String): String = {
    MessageParseUtils.getHeloMsg(userName)
  }

  def getHeloReply(msg: String): String = {
    MessageParseUtils.getHeloReply(msg)
  }

  def getPxrMsg(): String = {
    MessageParseUtils.getPxrMsg()
  }

  def getJoinMsg(): String = {
    MessageParseUtils.getJoinMsg()
  }

  def getRejoinMsg(msg: String): String = {
    MessageParseUtils.getRejoinMsg(msg)
  }

  def getIam(msg: String): Int = {
    state.iam = MessageParseUtils.getIam(msg)
    state.iam
  }

  def getGokMsg(): String = {
    MessageParseUtils.getGokMsg()
  }

  def getNextReadyMsg(): String = {
    MessageParseUtils.getNextReadyMsg()
  }

  def getByeMsg(): String = {
    MessageParseUtils.getByeMsg()
  }

  def parseTaikyokuMsg(msg: String): Unit = {
    val content = MessageParseUtils.extractMsg(msg)
    oya = content.split(" ").apply(1).trim.drop("oya=\"".length).dropRight("\"".length).toInt
    logger.debug(msg)
  }

  def parseNoopAction(): String = {
    actionHelper.parseNoopAction()
  }

  def genReachMsg(action: Int): String = {
    val msg = actionHelper.genReachMsg(action)
    turnOffReachAction()

    msg
  }

  def genDropOnlyMsg(action: Int): String = {
    actionHelper.genDropOnlyMsg(action)
  }

  def genStealActionMsg(action: Int): String = {
    logger.debug("deal with steal " + action)
    actionHelper.genStealActionMsg(action)
  }

  def isStealResponse(): Boolean = {
    MessageParseUtilsImpl.isStealResponse(state.state)
  }

  def isTerminalMsg(msg: String): Boolean = {
    MessageParseUtils.isTerminalMsg(msg)
  }

  def requiresAction(msg: String): Boolean = {
    msgHelper.requiresAction(msg)
  }

  def genDropAfterReach(msg: String): String = {
    actionHelper.genDropAfterReach(msg)
  }

  def genAcceptReply(msg: String): ActionResponse = {
    actionHelper.genAcceptReply(msg)
  }

  def generateMyActionReply(msg: String): ActionResponse = {
    actionHelper.generateMyActionReply(msg)
  }

  def generatePeerActionReply(msg: String): ActionResponse = {
    actionHelper.generatePeerActionReply(msg)
  }

  def genDropActionReply(): ActionResponse = {
    actionHelper.genDropActionReply()
  }


  def parseNoReplyMsg(msg: String): Unit = {
    msgHelper.parseNoReplyMsg(msg)
  }

  def genReachDropActionReply(): ActionResponse = {
    state.decideReach = true
    actionHelper.genReachDropActionReply()
  }

  def genReachResponse(msg: String): ActionResponse = {
    actionHelper.genReachResponse(msg)
  }

  def genAbortResponse(): ActionResponse = {
    actionHelper.genAbortResponse()
  }

  def isDropAction(action: Int): Boolean = {
    MessageParseUtilsImpl.isDropAction(action)
  }

  def isStealAction(action: Int): Boolean = {
    MessageParseUtilsImpl.isStealAction(action)
  }

  def isRonIndicator(msg: String): Boolean = {
    val isRon = MessageParseUtils.isRonIndicator(msg)

    if (isRon) {
      logger.debug("Going to ron " + msg)
    }

    isRon
  }

  def isReachIndicator(msg: String): Boolean = {
    MessageParseUtils.isReachIndicator(msg)
  }

  def genRonMsg(msg: String): String = {
    actionHelper.genRonMsg(msg)
  }

  def getGameReward(msg: String): Int = {
    MessageParseUtilsImpl.getGameReward(state.iam, msg)
  }

  def genTermActionReply(msg: String, tourEnd: Boolean): ActionResponse = {
    logger.info("Terminate " + msg)
    actionHelper.genTermActionReply(msg, tourEnd)
  }

  def parseGameEndMsg(msg: String): Int = {
    logger.info("Game end " + msg)
    msgHelper.parseGameEndMsg(msg)
  }

  def genGameEndReply(reward: Int, tourEnd: Boolean): ActionResponse = {
    actionHelper.genGameEndReply(reward, tourEnd)
  }

}
