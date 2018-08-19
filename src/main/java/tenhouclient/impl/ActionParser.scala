package tenhouclient.impl

import akka.event.slf4j.Logger
import tenhouclient.conn.msgs.ActionResponse
import org.deeplearning4j.gym.StepReply
import org.nd4j.linalg.api.ndarray.INDArray
import tenhouclient.utils.MessageParseUtils._

import tenhouclient.utils.TenhouConsts._
import tenhouclient.impl.ImplConsts._

class ActionParser(innerState: MdpInnerState) {

  private val logger = Logger("ActionParser")


  type ReplyType = StepReply[INDArray]

//  val pongActionFlag: Int = 1
//  val chowActionFlag: Int = 4
//  val kanActionFlag: Int = 2
//  val reachActionFlag: Int = 32
//  val ankanActionFlag: Int = 3
  //  val kaKanActionFlag: Int = 16





  def getDropTile(actionTile: Int): Int = {
    val rawState = innerState.rawState

    var tile = actionTile * NumPerTile

    val tiles = for (i <- tile until tile + NumPerTile if rawState(i) > 0) yield i
    logger.debug("drop tiles " + tiles.mkString(", "))
    if (tiles.length == 1) {
      tile = tiles.head
    }else {
      try {
        tile = tiles.filter(c => !MessageParseUtilsImpl.isAka(c)).head
      }catch {
        case e: Throwable =>
          logger.debug("Drop failed " + tile)
          logger.debug(tiles.mkString(","))
          val fTiles = tiles.filter(c => !MessageParseUtilsImpl.isAka(c))
          logger.debug(fTiles.mkString(","))
          logger.debug(fTiles.length + "")
          logger.debug(fTiles.head + "")

          for(i <- rawState.indices) {
            logger.debug(i + ": " + rawState(i) + ", ")
          }
          logger.debug("")
          throw e
      }
    }

    tile
  }

  def clearTry(): Unit = {
    val state = innerState.state
    for (i <- state.indices) {
      state(i) = state(i).asInstanceOf[Int].asInstanceOf[Double]
    }
  }

  def parseNoopAction(): String = {
    clearTry()
    getLogMsg("N ")
  }


  def generateDropActions(): Array[Int] = {
    val state = innerState.state

    val actions = Array.fill[Int](ActionLenWoAccept)(0)
    for (i <- 0 until TileNum) {
      if ((state(i).asInstanceOf[Int] & ExtraValueFlag) > 0) {
        actions(i) = 1
      }
    }

    actions
  }

  //  val tryValue: Double = 0.5

  //  private def genTState(msg: String): Unit = {
  //    if (msg.contains(" t=\"" + reachActionFlag + "\"")){
  //      innerState.state(PeerReachIndex) = tryValue
  //    }
  //  }

  def genAbortResponse(): ActionResponse = {
    val reply = new StepReply[INDArray](MessageParseUtilsImpl.generateIND(innerState), DefaultReward, true, null)
    new ActionResponse(reply, InvalidAction, 0, false, true)
  }

  def genReachResponse(msg: String): ActionResponse = {
    val tile = extractMsg(msg).split(" ").map(_.trim).apply(0).drop(1).trim.toInt
    MessageParseUtilsImpl.acceptTile(innerState, tile)

    innerState.state(PeerReachIndex) = reachValue
    innerState.inReachAction = true
    val reply = new StepReply[INDArray](MessageParseUtilsImpl.generateIND(innerState), DefaultReward, false, null)
    new ActionResponse(reply, InvalidAction, 0) //random tile
  }

  //TODO: Ankan may happen at this point
  def generateMyActionReply(msg: String): ActionResponse = {
    val tile = extractMsg(msg).split(" ").map(_.trim).apply(0).drop(1).trim.toInt
    MessageParseUtilsImpl.acceptTile(innerState, tile)

    val reply = new StepReply[INDArray](MessageParseUtilsImpl.generateIND(innerState), DefaultReward, false, null)
    new ActionResponse(reply, InvalidAction, tile)
  }

  private def getTryValue(t: Int): Double = {
    var tryValue: Double = 0
    if ((t & 1) > 0) tryValue += pongValue
    if ((t & 4) > 0) tryValue += chowValue

    tryValue
  }

  def generatePeerActionReply(msg: String): ActionResponse = {
    val tile = extractMsg(msg).split(" ").map(_.trim).apply(0).drop(1).trim.toInt
    MessageParseUtilsImpl.updateBoard(tile, innerState) //TODO: Good to update in this line? YES

    val t = extractMsg(msg).split(" ").map(_.trim).apply(1).drop("t=\"".length).dropRight("\"".length).toInt
    logger.debug("-------------> Get t = " + t)
    val tryValue = getTryValue(t)
    innerState.state(tile / NumPerTile) += tryValue

    val reply = new StepReply[INDArray](MessageParseUtilsImpl.generateIND(innerState), DefaultReward, false, null)
    logger.debug("Get tile for action: " + tile)
    new ActionResponse(reply, InvalidAction, tile)
  }


  def genTermActionReply(msg: String,  tourEnd: Boolean): ActionResponse = {
    msg match {
      case m if m.contains("AGARI") =>
        getAgariReply(msg, tourEnd)
      case m if m.contains("RYUUKYOKU") =>
        getRyuukyokuReply(msg, tourEnd)
      case _ =>
        logger.debug("Received unexpected terminal message " + msg)
        null
    }
  }

  def getAgariReply(msg: String, tourEnd: Boolean): ActionResponse = {
    val iam = innerState.iam

    val items = extractMsg(msg).split(" ").map(_.trim)
    var reward = DefaultReward

    items.foreach(item => {
      if (item.startsWith("sc")) {
        reward = item.drop("sc=\"".length).dropRight("\"".length).split(",")(iam * 2 + 1).toInt
      }
      if (item.startsWith("who")) {
        val who = item.drop("who=\"".length).dropRight("\"".length).toInt
        if (who == iam) {
          val tile = (for (i <- items.indices if items(i).contains("machi")) yield items(i).drop("machi=\"".length).dropRight("\"".length).toInt).head
          MessageParseUtilsImpl.acceptTile(innerState, tile)
        } //TODO: Suppose iam = 0
      }
    })

    val reply = new ReplyType(MessageParseUtilsImpl.generateIND(innerState), reward, true, null)
    new ActionResponse(reply, InvalidAction, 0, false, tourEnd) //tile is nonsense
  }

  def getRyuukyokuReply(msg: String, tourEnd: Boolean): ActionResponse = {
    val content = extractMsg(msg)
    val items = content.split(" ").map(_.trim)
    var reward = DefaultReward
    for (i <- items.indices) {
      if (items(i).startsWith("sc")) {
        reward = items(i).drop("sc=\"".length).dropRight("\"".length).split(",").map(_.trim).apply(1).toInt
      }
    }

    val reply = new ReplyType(MessageParseUtilsImpl.generateIND(innerState), reward, true, null)
    new ActionResponse(reply, InvalidAction, 0, false, tourEnd) //tile is nonsense
  }

  def genDropActionReply(): ActionResponse = {
    val reply = new StepReply[INDArray](MessageParseUtilsImpl.generateIND(innerState), DefaultReward, false, null)
    new ActionResponse(reply, InvalidAction, 0) //tile is nonsense
  }

  def genReachDropActionReply(): ActionResponse = {
    clearTry()
    innerState.state(PeerReachIndex) = ReachStep1
    val reply = new StepReply[INDArray](MessageParseUtilsImpl.generateIND(innerState), ReachReward, false, null)

    new ActionResponse(reply, REACHWoAccept, 0, true)
  }

  def genReachMsg(tileAction: Int): String = {
    logger.debug("+++++++++++++++++++++++++++++++++++ Is reach? " + innerState.decideReach)
    val dropTile = getDropTile(tileAction)

    if(innerState.decideReach) {
      innerState.state(PeerReachIndex) = ReachStep2
      getLogMsg("REACH hai=\"" + dropTile + "\"")
      //+ " | " + getLogMsg("D p=\"" + dropTile + "\"")
    }else {
      innerState.state(PeerReachIndex) = 0
      //      getLogMsg("N ") + " | " +
      getLogMsg("D p=\"" + dropTile + "\"")
    }
  }

  def genDropOnlyMsg(tileAction: Int): String = {
    val dropTile = getDropTile(tileAction)

    getLogMsg("D p=\"" + dropTile + "\"")
  }

  def genDropAfterReach(msg: String): String = {
    val content =extractMsg(msg)
    val tile = content.split(" ").map(_.trim).apply(0).drop("T".length).toInt

    getLogMsg("D p=\"" + tile + "\"")
  }

  def getNextReadyMsg: String = getLogMsg("NEXTREADY")

  def genStealActionMsg(action: Int): String = {
    action match {
      case PongWoAccept => genPongActionMsg(action)
      case ChowWoAccept => genChowActionMsg(action)
      case MinKanWoAccept =>
        getLogMsg("N ")
      case AnKanWoAccept =>
        logger.debug("Received unexpected action request ")
        ""
      case KaKanWoAccept => getLogMsg("N ")
      case _ =>
        logger.debug("Received unexpected action request " + action)
        ""
    }
  }

  private[this] def getStealTile(): Int = {
    (for (i <- 0 until TileNum if math.floor(innerState.state(i)) != innerState.state(i)) yield i).head
  }

  private[this] def genChowActionMsg(action: Int): String = {
    val rawState = innerState.rawState
    val tile = getStealTile()
    logger.debug("-------------------------> Get steal tile " + tile)
    clearTry()

    val acceptTile = tile
    val candidates = for (i <- math.max(acceptTile - 2, 0) to math.min(acceptTile, 27 - 1 - 2) if canChow(i, tile))
      yield i

    logger.debug(innerState.state.mkString(", "))
    logger.debug(innerState.rawState.mkString(", "))
    logger.debug("--------------------------> Candidates " + candidates.mkString(", "))
    val index = chowRandom.nextInt(candidates.length)

    val candidate = candidates(index)
    logger.debug("Get candidate " + candidate)
    val tiles = for (i <- 0 until 3 if (i + candidate) != tile) yield candidate + i
    logger.debug(tiles.mkString(","))



    val tile0 = (for (i <- tiles(0) * NumPerTile until (tiles(0) + 1)* NumPerTile if rawState(i) > 0) yield  i).head
    val tile1 = (for (i <- tiles(1) * NumPerTile until (tiles(1) + 1)* NumPerTile if rawState(i) > 0) yield  i).head

    getLogMsg("N type=\"3\" hai0=\"" + tile0 + "\" hai1=\"" + tile1 + "\"")
  }

  private[this] def genPongActionMsg(action: Int): String = {
    val rawState = innerState.rawState

    val tile = getStealTile() // It is rawTile / 4
    clearTry()

    val candidate = tile * NumPerTile
    val tiles = for (i <- candidate until candidate + NumPerTile if rawState(i) > 0) yield  i

    val pongTiles = Array[Int](tiles(0), tiles(1))
    //    for (i <- 0 until 2) {
    //      pongTiles(i) = tiles(i)
    //    }

    if(tiles.length > 2) {
      if (MessageParseUtilsImpl.isAka(tiles(2))) {
        val alterIndex = (for (i <- pongTiles.indices if !MessageParseUtilsImpl.isAka(pongTiles(i))) yield i).head
        pongTiles(alterIndex) = tiles(2)
      }
    }

    //    val dropTiles = pongTiles.filter(t => t != tile)
    logger.debug(innerState.state.mkString(", "))
    logger.debug(innerState.rawState.mkString(", "))
    logger.debug("tiles: " + tiles.mkString(", "))

    getLogMsg("N type=\"1\" hai0=\"" + pongTiles(0) + "\" hai1=\"" + pongTiles(1) + "\"")
  }

  def ifTileExist(state: Array[Int], tile: Int): Boolean = {
    (state(tile) & ExtraValueFlag) > 0
  }

  private val chowRandom = new scala.util.Random(37)

  //TODO: Make it function in function
  private def canChow(candidate: Int, actionTile: Int): Boolean = {
    //    var rc: Boolean = true

    //    (0 until 3).count(i => {
    //      (innerState.state(i + candidate).toInt & ExtraValueFlag) > 0 || innerState.state(i + candidate).toInt == actionTile
    //    }) >= 2

    (0 until 3).filter(i => {(i + candidate) != actionTile}).count(i => {
      ((innerState.state(i + candidate).toInt & ExtraValueFlag) > 0) &&
        ((i + candidate) / NumPerSort == actionTile / NumPerSort)}) >= 2

  }



  def genAcceptReply(msg: String): ActionResponse = {
    val state = innerState.state

    if (!msg.contains("t=")) {
      genDropActionReply()
    }else {
      val reply = new StepReply[INDArray](MessageParseUtilsImpl.generateIND(innerState), DefaultReward, false, null)

      new ActionResponse(reply, InvalidAction, 0, true)
    }
  }

  def genRonMsg(msg: String): String = {
    val indicator = getRonIndicator(msg)
    val replyType = getRonType(indicator)

    getLogMsg("N type=\"" + replyType + "\"")
  }

  def genGameEndReply(reward: Int, tourEnd: Boolean): ActionResponse = {
    val reply = new ReplyType(MessageParseUtilsImpl.generateIND(innerState), reward, true, null)
    new ActionResponse(reply, InvalidAction, 0, false, tourEnd) //tile is nonsense
  }

}
