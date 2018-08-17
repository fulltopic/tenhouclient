package client

import conn.msgs.ActionResponse
import utils.TenhouConsts.TileNum

trait MsgHelper {
  def iam(): Int

  def isInReachAction(): Boolean

//  def turnOffReachAction(): Unit

  def reached(): Boolean

  def getKAMsg(): String

  def getHeloMsg(userName: String): String

  def getHeloReply(msg: String): String

  def getPxrMsg(): String

  def getJoinMsg(): String

  def getRejoinMsg(msg: String): String

  def getIam(msg: String): Int

  def getGokMsg(): String

  def getNextReadyMsg(): String

  def getByeMsg(): String

  def parseTaikyokuMsg(msg: String): Unit

  def parseNoopAction(): String

  def genReachMsg(action: Int): String

  def genDropOnlyMsg(action: Int): String

  def genStealActionMsg(action: Int): String

  def isStealResponse(): Boolean

  def isTerminalMsg(msg: String): Boolean

  def requiresAction(msg: String): Boolean

  def genDropAfterReach(msg: String): String

  def genAcceptReply(msg: String): ActionResponse

  def generateMyActionReply(msg: String): ActionResponse

  def generatePeerActionReply(msg: String): ActionResponse

  def genDropActionReply(): ActionResponse

  def parseNoReplyMsg(msg: String): Unit

  def genReachDropActionReply(): ActionResponse

  def genReachResponse(msg: String): ActionResponse

  def genAbortResponse(): ActionResponse

  def isDropAction(action: Int): Boolean

  def isStealAction(action: Int): Boolean

  def isRonIndicator(msg: String): Boolean

  def isReachIndicator(msg: String): Boolean

  def genRonMsg(msg: String): String

  def getGameReward(msg: String): Int

  def genTermActionReply(msg: String, tourEnd: Boolean): ActionResponse

  def parseGameEndMsg(msg: String): Int

  def genGameEndReply(reward: Int, tourEnd: Boolean): ActionResponse

}
