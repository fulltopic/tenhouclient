package tenhouclient.impl

import akka.event.slf4j.Logger
import org.nd4j.linalg.api.ndarray.INDArray
import org.nd4j.linalg.factory.Nd4j
import tenhouclient.utils.MessageParseUtils
import MdpInnerState._
import ImplConsts._

object MessageParseUtilsImpl {
  private val logger = Logger("MessageParseUtilsImpl")


  val jsonActionKey = "actions"
  val jsonTileKey = "tile"
  private[this] val keys = Set[String]("TAIKYOKU", " INIT", "T", "D", "U", "E", "V", "F", "W", "G", "N", "RYUUKYOKU", "AGARI", "REACH", "DORA")
  private[this] val sceneKeys = Set[String]("HELO", "JOIN", "REJOIN", "UN", "LN", "GO", "PROF")
  private[this] val actionTitle = Map[Int, (String, String)](0 -> ("T", "D"), 1 -> ("U", "E"), 2 -> ("V", "F"), 3 -> ("W", "G"))
  private[this] val gameMsgSet = scala.collection.immutable.Set[String]("INIT", "DORA", "REACH", "AGARI", "N", "RYUUKYOKU", "REINIT", "SAIKAI", "TAIKYOKU") //, "PROF"
  val dropKeyPattern = "[e, f, g, E, F, G][0-9]+"
  val acceptKeyPattern = "[u, v, w, U,V,W]"
  val myAcceptPattern = "[T, t][0-9]+"
  val myDropPattern = "[D][0-9]+"
  val tileKeyPattern = "[d, u, e, v, f, w, t, g, D,U,E,V,F,W,T,G][0-9]+" //efgd for drop, uvwt for accept
  val ronActionFlags = Set[Int](8, 9, 10, 11, 12, 13, 15, 16)



  def updateBoard(tile: Int, innerState: MdpInnerState): Unit = {
    val state = innerState.state
    state(tile / NumPerTile + TileNum) += 1
  }

  def acceptTile(innerState: MdpInnerState, tile: Int): Unit = {
    val state = innerState.state
    val doraValue = innerState.doraValue
    val rawState = innerState.rawState

    state(tile / NumPerTile) += 1 + doraValue(tile / NumPerTile) + ImplConsts.AkaValues.getOrElse(tile, 0)
    rawState(tile) += 1

    logger.debug("======================================> Accepted " + tile)
  }

  def dropTile(innerState: MdpInnerState, tile: Int): Unit = {
    val state = innerState.state
    val doraValue = innerState.doraValue
    val rawState = innerState.rawState

    state(tile / NumPerTile) -= 1 + doraValue(tile / NumPerTile) + ImplConsts.AkaValues.getOrElse(tile, 0)
    rawState(tile) -= 1

    logger.debug("======================================> Dropped " + tile)
  }

  def fixTile(innerState: MdpInnerState, tile: Int): Unit = {
    val state = innerState.state

    state(tile / NumPerTile) += ImplConsts.MValue - 1

    innerState.rawState(tile) -= 1 //This tile is impossible to be dropped
  }

  def reached(innerState: MdpInnerState): Boolean = {
    innerState.state(ImplConsts.PeerReachIndex) > 0
  }

  def isDropAction(action: Int): Boolean = {
    Range(0, TileNum).contains(action)
  }

  def isStealAction(action: Int): Boolean = {
    action >= ImplConsts.ChowWoAccept && action <= ImplConsts.RonWoAccept
  }

  def getDora(hai: Int): Int = {
    hai match {
      case tile if tile >= 0 && tile < 27 =>
        val tmp = tile / 9
        (tile + 1) % 9 + tmp * 9
      case tile if tile >= 27 && tile < 31 =>
        val tmp = tile - 27
        (tmp + 1) % 4 + 27
      case _ =>
        val tmp = hai - 31
        (hai + 1) % 3 + 31
    }
  }

  // All input hai for parsexx functions are original hai, not / 4
  def parseDora(hai: Int, innerState: MdpInnerState): Unit = {
    val state = innerState.state
    val doraValue = innerState.doraValue

    if (hai >= 0) {
      val doraHai = getDora(hai / NumPerTile)
      doraValue(doraHai) = ImplConsts.DoraValue
      if (state(doraHai) > 0) {
        var tileNum = state(doraHai).asInstanceOf[Int] & ImplConsts.ExtraValueFlag
        tileNum += state(doraHai).asInstanceOf[Int] / ImplConsts.MValue // fixed
        state(doraHai) += tileNum * ImplConsts.DoraValue
      }
    }
  }

  def getWhoFromN(msg: String): Int = {
    val content = MessageParseUtils.extractMsg(msg)

    content.split(" ").map(_.trim).apply(1).drop("who=\"".length).dropRight("\"".length).toInt
  }


  def getKeyInMsg(msg: String): String = {
    msg.trim.drop(1).dropRight(2).trim.split(" ").head
  }

  def getMsgItems(msg: String): Seq[String] = {
    MessageParseUtils.extractMsg(msg).split(" ").map(_.trim)
  }

  def dropQuotation(msg: String, title: String): String = {
    msg.drop((title + "=\"").length).dropRight("\"".length)
  }

  def generateIND(innerState: MdpInnerState) = {
    val state = innerState.state

    val ind = Nd4j.zeros(ImplConsts.PeerStateLen)
    for (i <- state.indices) {
      ind.putScalar(i, state(i))
    }

    ind
  }

  val ronActions = Set[Int](16, 48, 64, 8, 9, 10, 11, 12, 13, 15)
  def isRonIndicator(msg: String): Boolean = {
    var isRon: Boolean = false
    logger.debug("Get ron action: ")

    if (msg.startsWith("<T") && msg.contains(" t=")) {
      val action = getRonIndicator(msg)
      logger.debug("Get ron action: " + action)

      isRon = ronActions.contains(action)
    }else {
      val items = MessageParseUtils.extractMsg(msg).split(" ").map(_.trim)
      logger.debug("Get ron action: " + items.mkString(","))

      if (items.length == 2) {
        if (items(0).matches(dropKeyPattern)) {
          val actionType = items(1).drop("t=\"".length).dropRight("\"".length).toInt
          if (ronActions.contains(actionType)) {
            isRon = true
          }
        }
      }
    }

    isRon
  }

  def isReachIndicator(msg: String): Boolean = {
    msg.contains("<T") && msg.contains("t=\"32\"")
  }

  def isStealResponse(state: Array[Double]): Boolean = {
    for (i <- 0 until TileNum) {
      if (state(i) - state(i).asInstanceOf[Int].asInstanceOf[Double] > 0)
        return true
    }

    false
  }

  def getRonIndicator(msg: String): Int = {
    MessageParseUtils.extractMsg(msg).split(" ").map(_.trim).apply(1).drop("t=\"".length).dropRight("\"".length).toInt
  }

  def getRonType(indicator: Int): Int = {
    if (!ronActions.contains(indicator)) {
      logger.debug("!!!!!!!!!!!!!!!!!!!!!!!! Invalid ron indicator ! " + indicator)
      0
    }else {
      indicator match {
        case 16 => 7
        case 48 => 7
        case 64 => 9
        case _ => 6
      }
    }
  }

  private def getStealTiles(lastState: INDArray): Array[Int] = {
    (for (i <- 0 until TileNum if math.floor(lastState.getDouble(i)) < lastState.getDouble(i)) yield  i).toArray
  }

  val orphanTiles = Array[Int](0, 8, 9, 17, 18, 26, 27, 28, 29, 30, 31, 32, 33).toSet
  private def is13Orphan(nums: Array[Int]): Boolean = {
    (for (i <- nums.indices if nums(i) > 0 && orphanTiles.contains(i)) yield i).length >= 12
  }
  private def get13OrphanTiles(nums: Array[Int]): Set[Int] = {
    (for (i <- nums.indices if nums(i) > 0 && !orphanTiles.contains(i)) yield i).toSet
  }

  def is7Pairs(nums: Array[Int]): Boolean = {
    var count = 0
    nums.foreach(n => {
      if (n >= 2) count += 1
    })

    count == 6
  }

  private def get7PairTiles(nums: Array[Int]): Set[Int] = {
    (for (i <- nums.indices if (nums(i) % 2) > 0) yield i).toSet
  }

  private def tryChowReach(nums: Array[Int], hasPair: Boolean): Set[Int] = {
    logger.debug("----------------------------> tryChowReach " + hasPair + " :" + nums.mkString(", "))

    var index: Int = -1
    var indices = (for (i <- 0 until 7 if nums(i) > 0 && nums(i + 1) > 0 && nums(i + 2) > 0) yield i).sorted
    if (indices.nonEmpty){
      var rcSet = Set.empty[Int]
      for (i <- indices.indices) {
        index = indices(i)
        nums(index) -= 1
        nums(index + 1) -= 1
        nums(index + 2) -= 1
        val rc = try4GroupTiles(nums, hasPair)
        rcSet = rcSet ++ rc
        nums(index) += 1
        nums(index + 1) += 1
        nums(index + 2) += 1
      }
      rcSet
    }else {
      indices = (for (i <- 9 until 16 if nums(i) > 0 && nums(i + 1) > 0 && nums(i + 2) > 0) yield i).sorted

      if (indices.nonEmpty) {
        var rcSet = Set.empty[Int]
        for (i <- indices.indices) {
          index = indices(i)
          nums(index) -= 1
          nums(index + 1) -= 1
          nums(index + 2) -= 1
          val rc = try4GroupTiles(nums, hasPair)
          rcSet = rcSet ++ rc
          nums(index) += 1
          nums(index + 1) += 1
          nums(index + 2) += 1
        }
        rcSet
      }else {
        indices = (for (i <- 18 until 25 if nums(i) > 0 && nums(i + 1) > 0 && nums(i + 2) > 0) yield i).sorted
        if (indices.nonEmpty) {
          var rcSet = Set.empty[Int]
          for (i <- indices.indices) {
            index = indices(i)
            nums(index) -= 1
            nums(index + 1) -= 1
            nums(index + 2) -= 1
            val rc = try4GroupTiles(nums, hasPair)
            rcSet = rcSet ++ rc
            nums(index) += 1
            nums(index + 1) += 1
            nums(index + 2) += 1
          }
          rcSet
        }else {
          Set.empty[Int]
        }
      }
    }
  }

  private def tryPongReach(nums: Array[Int], hasPair: Boolean): Set[Int] = {
    logger.debug("----------------------------> tryPongReach " + hasPair + " :" + nums.mkString(", "))

    var index: Int = 0
    while (index < nums.length && nums(index) < 3 ) index += 1

    if (index >= nums.length) Set.empty[Int]
    else {
      nums(index) -= 3
      val rc = try4GroupTiles(nums, hasPair)
      nums(index) += 3
      rc
    }
  }

  private def tryPair(nums: Array[Int], hasPair: Boolean): Set[Int] = {
    logger.debug("----------------------------> tryPair " + hasPair + " :" + nums.mkString(", "))

    if (hasPair) return Set.empty[Int]

    var index = 0
    while (index < TileNum && nums(index) < 2) index += 1

    if (index >= TileNum) {
      Set.empty[Int]
    }else {
      nums(index) -= 2
      val rc = try4GroupTiles(nums, true)
      nums(index) += 2
      rc
    }
  }

  private def try4GroupTiles(nums: Array[Int], hasPair: Boolean): Set[Int] = {
    logger.debug("----------------------------> try4group " + hasPair + " :" + nums.mkString(", "))

    val tileExistCount = nums.sum
    tileExistCount match {
      case 0 =>
        logger.debug("Failed to try")
        Set.empty[Int]
      case 1 =>
        logger.debug("Failed to try")
        Set.empty[Int]
      case 2 => //pair
        val rc = (for (i <- nums.indices if nums(i) > 0) yield i).toSet
        logger.debug("Wait for pair " + rc.mkString(", "))
        rc
      case 3 =>
        val remainTiles = (for (i <- nums.indices if nums(i) > 0) yield i).toList.sorted
        logger.debug("Remain 3: " + remainTiles.mkString(", "))
        if (remainTiles.length == 2) { //pong
          logger.debug("Wait for pong " + remainTiles.filter(r => nums(r) == 1).toSet.mkString(", "))
          return remainTiles.filter(r => nums(r) == 1).toSet
        }else { //chow
          def mayChow(tile1: Int, tile2: Int): Boolean = {
            tile1 / NumPerSort == tile2 / NumPerSort && math.abs(tile1 - tile2) <= 2 && math.max(tile1, tile2) < 27
          }

          if (mayChow(remainTiles(0), remainTiles(1))) {
            logger.debug("Wait for chow " + remainTiles(2))
            return Set[Int](remainTiles(2))
          }
          if (mayChow(remainTiles(1), remainTiles(2))) {
            logger.debug("Wait for chow " + remainTiles(0))
            return Set[Int](remainTiles(0))
          }

        }
        logger.debug("Failed to try")
        Set.empty[Int] //impossible
      case _ =>
        tryChowReach(nums, hasPair) ++ tryPongReach(nums, hasPair) ++ tryPair(nums, hasPair)
    }
  }

  private def get4GroupTiles(nums: Array[Int]): Set[Int] = {
    try4GroupTiles(nums, false)
  }

  def getReachDropTile(state: INDArray): Set[Int] = {
    val tileNums: Array[Int] = (for (i <- 0 until TileNum) yield state.getDouble(i).toInt & ImplConsts.ExtraValueFlag).toArray
    var reachTiles = Set.empty[Int]

    logger.debug("----------------------> " + tileNums.mkString(", "))
    tileNums match {
      case nums: Array[Int] if is13Orphan(nums) =>
        logger.debug("--------------------------> 13 ")
        reachTiles = get13OrphanTiles(nums)
      case nums: Array[Int] if is7Pairs(nums) =>
        logger.debug("--------------------------> 7 ")
        reachTiles = get7PairTiles(nums)
      case _ =>
        logger.debug("--------------------------> 4")
        reachTiles = get4GroupTiles(tileNums)
    }
    logger.debug("Reach drop tiles " + reachTiles.mkString(", "))
    reachTiles
  }

  def getAvailableActions(lastState: INDArray): List[Int] = {
    logger.debug("lastState" + lastState)
    var actions = List.empty[Int]

    // Reach
    if (lastState.getDouble(ImplConsts.PeerReachIndex) > math.floor(lastState.getDouble(ImplConsts.PeerReachIndex))) {
      logger.debug("----------------> Reach action")
      //TODO: This is to test reach
      actions = List[Int](ImplConsts.REACHWoAccept)
    }else if(lastState.getDouble(ImplConsts.PeerReachIndex) == ReachStep1) {
      actions = getReachDropTile(lastState).toList
    }
    else if(getStealTiles(lastState).length > 0) { // Steal
      val candidates = getStealTiles(lastState)
      logger.debug("----------------> Get steal tiles " + candidates.mkString(", "))

      val stealTile = candidates.head
      val delta = lastState.getDouble(stealTile) - math.floor(lastState.getDouble(stealTile))
      logger.debug("----------------> Get steal tile and delta " + stealTile + ", " + delta)
      if (delta == pongValue) {
        actions = actions :+ ImplConsts.PongWoAccept
      } else if (delta == chowValue) {
        actions = actions :+ ImplConsts.ChowWoAccept
      }else if (delta == chowValue + pongValue) {
        actions = actions :+ ImplConsts.PongWoAccept
        actions = actions :+ ImplConsts.ChowWoAccept
      }
      actions = actions :+ ImplConsts.NOOPWoAccept
    }else { //Drop
      logger.debug("----------------------> Drop actions")
      actions = (for (i <- 0 until TileNum if (lastState.getDouble(i).toInt & ImplConsts.ExtraValueFlag) > 0) yield i).toList
    }

    logger.debug("Available actions: ", actions.mkString(","))

    actions
  }

  def getLegalQAction(rawState: INDArray, qs: INDArray): org.nd4j.linalg.primitives.Pair[java.lang.Double, Integer] = {
    val legalActions = getAvailableActions(rawState).toSet
    var maxQ: Double = Double.MinValue
    var action: Int = 0

    for (i <- 0 until ImplConsts.ActionLenWoAccept) {
      if (legalActions.contains(i)) {
        val q = qs.getDouble(i)
        if (q > maxQ) {
          maxQ = q
          action = i
        }
      }
    }

    new org.nd4j.linalg.primitives.Pair(maxQ, action)
  }

  def dropTileFromMsg(msg: String): Int = {
    val content = MessageParseUtils.extractMsg(msg)
    content.split(" ").map(_.trim).apply(1).drop("hai=\"".length).dropRight("\"".length).toInt
  }

  def isMyReachIndicator(msg: String): Boolean = {
    //    logger.debug("Check if my reach indicator " + msg)
    var isStep1: Boolean = false

    if (msg.contains("REACH")) {
      val items = MessageParseUtils.extractMsg(msg).split(" ").map(_.trim)

      if (items.length == 3) {
        val whoStr = items(1)
        val stepStr = items(2)

        val who = whoStr.drop("who=\"".length).dropRight("\"".length).toInt
        val step = stepStr.drop("step=\"".length).dropRight("\"".length).toInt

        if (who == 0 && step == 1) {
          logger.info("-------------------------------------> I am reached!")
          isStep1 = true
        }
      }
    }

    isStep1
  }

  private def getAgariReward(iam: Int, msg: String): Int = {

    val items = MessageParseUtils.extractMsg(msg).split(" ").map(_.trim)
    var reward = ImplConsts.DefaultReward

    items.foreach(item => {
      if (item.startsWith("sc")) {
        reward = item.drop("sc=\"".length).dropRight("\"".length).split(",")(iam * 2 + 1).toInt
      }
    })

    reward
  }

  private def getRyuReward(iam: Int, msg: String): Int = {
    val content = MessageParseUtils.extractMsg(msg)
    val items = content.split(" ").map(_.trim)
    var reward = ImplConsts.DefaultReward
    for (i <- items.indices) {
      if (items(i).startsWith("sc")) {
        reward = items(i).drop("sc=\"".length).dropRight("\"".length).split(",").map(_.trim).apply(iam * 2 + 1).toInt
      }
    }

    reward
  }

  def getGameReward(iam: Int, msg: String): Int = {
    msg match {
      case m: String if m.contains("AGARI") => getAgariReward(iam, msg)
      case m: String if m.contains("RYUUKYOKU") => getRyuReward(iam, msg)
      case _ => ImplConsts.DefaultReward
    }
  }


  def isAka(tile: Int): Boolean = {
    ImplConsts.AkaValues.getOrElse(tile, 0) > 0
  }

}
