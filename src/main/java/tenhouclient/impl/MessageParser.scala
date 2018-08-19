package tenhouclient.impl

import akka.event.slf4j.Logger
import tenhouclient.utils.MessageParseUtils._

import tenhouclient.impl.ImplConsts._
import tenhouclient.utils.TenhouConsts._

class MessageParser(innerState: MdpInnerState)  {
  private[this] val logger = Logger("MessageParser")

  val tileHead = Set[String]("<F", "<E", "<G", "<f", "<e", "<g")
  def requiresAction(msg: String): Boolean = {
    //    logger.debug("-------------------> Requires action: " + msg)
    val iam = innerState.iam
    msg match {
      case s if s.startsWith("<T") => true
      case s if tileHead.foldLeft[Boolean](false)((a, b) => {a || s.startsWith(b)}) && s.contains("t") => true
      case s if s.contains("INIT") => false

      case s if isTerminalMsg(s) => true
      // TODO: N with who == me
      case s if s.startsWith("<N") && getWhoFromN(msg) == iam => true
      case _ => false
    }
  }



  val acceptPeerPattern = "[e, f, g, E, F, G][0-9]+"
  val dropPeerPattern = "[u, v, w, U, V, W][0-9]+"
  val acceptMinePattern = "[T, t][0-9]+"
  val dropMinePattern = "[D, d][0-9]+"

  def parseNoReplyMsg(msg: String): Int = {
    val key = getKeyInMsg(msg)
    key match {
      case "INIT" => parseInitMsg(msg)
      case "REINIT" => parseReinitMsg(msg)
      case "REACH" => parseReachMsg(msg)
      case "N" => parseNMsg(msg)

      case k if k.split(" ").head.matches(tileKeyPattern) => parseTileMsg(key.split(" ").head)
      case _ => DefaultReward
    }
  }

  def parseReachMsg(msg: String): Int = {
    logger.debug("Processed reach message " + msg)
    val state = innerState.state

    val items = msg.split(" ").map(_.trim)
    val who = items(1).drop("who=\"".length).dropRight("\"".length).toInt

    if(who == innerState.iam) {
      logger.debug("I am reached " + who + " " + innerState.iam)
      state(PeerReachIndex) = ReachStep1
    }

    items match {
      case elems: Array[String] if elems.length == 3 =>
        // step1
        state(PeerCommonReach + who) = ReachStep1
      case elems: Array[String] if elems.length == 4 =>
        // step2
        state (PeerCommonReach + who) = ReachStep2
      case _ => logger.debug("Unexpected message " + msg)
    }
    DefaultReward //Reach had happened
  }

  def parseTileMsg(key: String): Int = {
    key match {
      case k if k.matches(dropPeerPattern) =>
        MessageParseUtilsImpl.updateBoard(getTileTile(key), innerState)
        DefaultReward
      case k if k.matches(acceptPeerPattern) => DefaultReward
      case k if k.matches(acceptMinePattern) =>
        logger.debug("-----------------------------------_>  Seemed received unexpected T message " + key)
        DefaultReward
      case k if k.matches(dropMinePattern) =>
        val dropTile = getTileTile(k)
        MessageParseUtilsImpl.dropTile(innerState, dropTile)
        MessageParseUtilsImpl.updateBoard(dropTile, innerState)
        DefaultReward //Seemed impossible
      case _ =>
        logger.debug("Failed to parse tile message " + key)
        DefaultReward
    }
  }

  private def initStates(): Unit = {
    val state = innerState.state
    val rawState = innerState.rawState

    for (i <- state.indices) {
      state(i) = 0
    }
    for (i <- innerState.rawState.indices) {
      rawState(i) = 0
    }
    for (i <- innerState.doraValue.indices) {
      innerState.doraValue(i) = 0
    }
  }

  def parseReinitMsg(msg: String): Int = {
    initStates()

    val reinitMsg = extractMsg(msg)
    val items = reinitMsg.split(" ").map(_.trim)

    var initMsg = "INIT"
    for (item <- items) {
      if (item.startsWith("seed") ||
        item.startsWith("ten") ||
        item.startsWith("oya") ||
        item.startsWith("hai")) {
        initMsg += " " + item
      }
    }
    logger.debug("Generated init message " + getLogMsg(initMsg).replace("\0", " "))
    parseInitMsg(getLogMsg(initMsg).replace("\0", " "))

    for (item <- items) {
      if (item.startsWith("kawa")) {
        val who = item.take("kawax".length).drop("kawa".length).toInt
        logger.debug("------------> Get who " + who)
        val dropTiles = item.drop("kawax=\"".length).dropRight("\"".length).split(",").map(_.toInt)
        //TODO: Suppose I am 0
        for (tile <- dropTiles) {
          // Reach
          if (tile == ReinitReach) {
            if (who == 0) {
              innerState.state(PeerReachIndex) = 1
            }else {
              innerState.state(PeerCommonReach + who) = 1
            }
          }else {
            MessageParseUtilsImpl.updateBoard(tile, innerState)
          }
        }
      }
    }
    for (item <- items) {
      if(item.startsWith("m")) {
        var who = item.take(2).drop(1).toInt
        val mValue = item.drop("mx=\"".length).dropRight("\"".length).split(",").map(_.trim).map(_.toInt)
        for (m <- mValue) {
          if (who == innerState.iam) {
            who = (who + 1) % PlayerNum //don't care my N
          }
          val nMsg = getLogMsg("<N who=\"" + who + "\" m=\"" + m + "\" ").replace("\0", " ")
          //          logger.debug("Parse nmsg " + nMsg)
          parseNMsg(nMsg)
        }
      }
    }

    DefaultReward
  }

  def parseInitMsg(msg: String): Int = {
    initStates()

    val items = msg.trim.drop(1).dropRight(2).trim.split(" ").map(_.trim)
    val haiMsg = items(4).drop("hai=\"".length).dropRight("\"".length)
    val tiles = haiMsg.split(",").map(_.toInt)
    tiles.foreach(tile => MessageParseUtilsImpl.acceptTile(innerState, tile))

    val doraHai = items(1).drop("seed=\"".length).dropRight("\"".length).split(",").map(_.trim).map(_.toInt).apply(DoraInSeed)
    MessageParseUtilsImpl.parseDora(doraHai, innerState)

    val oya = items(3).drop("oya=\"".length).dropRight("\"".length).toInt
    if (oya == innerState.iam) {
      innerState.state(PeerOyaIndex) = 1
    }

    DefaultReward
  }

  def parseChow(m: Int, who: Int) = {
    var chowTile = (m >> 10) & 63
    val r = chowTile % 3

    chowTile /= 3
    chowTile = chowTile / 7 * 9 + chowTile % 7
    chowTile *= 4

    val candidates = new Array[Int](3)
    candidates(0) = chowTile + ((m >> 3) & 3)
    candidates(1) = chowTile + 4 + ((m >> 5) & 3)
    candidates(2) = chowTile + 8 + ((m >> 7) & 3)

    val peers = new Array[Int](2)
    r match {
      case 0 =>
        chowTile = candidates(0)
        peers(0) = candidates(1)
        peers(1) = candidates(2)
      case 1 =>
        chowTile = candidates(1)
        peers(0) = candidates(0)
        peers(1) = candidates(2)
      case 2 =>
        chowTile = candidates(2)
        peers(0) = candidates(0)
        peers(1) = candidates(1)
    }

    //    if (reinit) {
    //      peers.foreach(peer => MessageParseUtils.acceptTile(innerState, peer))
    //    }

    if (who == innerState.iam) {
      MessageParseUtilsImpl.acceptTile(innerState, chowTile)
      MessageParseUtilsImpl.fixTile(innerState, chowTile)
      peers.foreach(t => MessageParseUtilsImpl.fixTile(innerState, t))
    }

    peers.foreach(peer => MessageParseUtilsImpl.updateBoard(peer, innerState))
  }

  def parsePong(m: Int, who: Int) = {
    val unused: Int = (m >> 5) & 3
    var tmp: Int = (m >> 9) & 127
    val r: Int = tmp % 3

    tmp /= 3
    tmp *= 4

    val selfHai: Array[Int] = new Array[Int](2)
    var count: Int = 0
    var idx: Int = 0

    for (i <- 0 until NumPerTile) {
      if (i != unused) {
        if (count != r) {
          selfHai(idx) = tmp + i
          idx += 1
        }
        count += 1
      }
    }

    val pongTile = tmp + r

    if (who == innerState.iam) {
      MessageParseUtilsImpl.acceptTile(innerState, pongTile)
      MessageParseUtilsImpl.fixTile(innerState, pongTile)
      selfHai.foreach(t => MessageParseUtilsImpl.fixTile(innerState, t))
    }

    selfHai.foreach(t => MessageParseUtilsImpl.updateBoard(t, innerState))
  }

  def parseKakan(m: Int, who: Int) = {
    val unused = (m >> 5) & 3
    var tmp = (m >> 9) & 127
    val r = tmp % 3
    tmp /= 3
    tmp *= 4

    val selfHai = Array.fill[Int](2)(0)
    var count: Int = 0
    var idx: Int = 0
    for (i <- 0 until NumPerTile) {
      if (i != unused) {
        if (count != r) {
          selfHai(idx) = tmp + i
          idx += 1
        }
        count += 1
      }
    }

    val kanTile = tmp + r
    val addTile = tmp + unused

    if (who == innerState.iam) {
      MessageParseUtilsImpl.acceptTile(innerState, kanTile)
      MessageParseUtilsImpl.fixTile(innerState, kanTile)
      MessageParseUtilsImpl.fixTile(innerState, addTile)
      selfHai.foreach(t => MessageParseUtilsImpl.fixTile(innerState, addTile))
    }

    MessageParseUtilsImpl.updateBoard(addTile, innerState)
    selfHai.foreach(t => MessageParseUtilsImpl.updateBoard(t, innerState))

  }

  def parseAnkan(m: Int, who: Int) = {
    var tile = (m >> 8) & 255
    tile = tile / 4 * 4

    // Had been accepted by T
    //TODO: Which t corresponding to ankan in <T ?
    if (who == innerState.iam) {
      for (i <- 0 until NumPerTile) {
        MessageParseUtilsImpl.fixTile(innerState, i + tile)
      }
    }

    for (i <- 0 until NumPerTile) {
      MessageParseUtilsImpl.updateBoard(i + tile, innerState)
    }
  }

  def parseKita(m: Int, who: Int) = {
    // Don't know how to with kita
  }

  //From github tenhou-visualizer-master
  def parseMinKan(m: Int, who: Int) = {

    val nakiHai = (m >> 8) & 255
    val haiFirst = nakiHai / 4 * 4
    val selfHai = Array.fill[Int](3)(0)
    var idx = 0
    for (i <- 0 until 3) {
      if (haiFirst + idx == nakiHai) idx += 1
      selfHai(i) = haiFirst + idx
      idx += 1
    }

    if (who == innerState.iam) {
      MessageParseUtilsImpl.acceptTile(innerState, nakiHai)
      MessageParseUtilsImpl.fixTile(innerState, nakiHai)
      selfHai.foreach(t => MessageParseUtilsImpl.fixTile(innerState, t))
    }

    selfHai.foreach(t => MessageParseUtilsImpl.updateBoard(t, innerState))
  }

  def parseNMsg(msg: String): Int = {
    val content = extractMsg(msg)
    val items = content.split(" ").map(_.trim)
    val who = items(1).drop("who=\"".length).dropRight("\"".length).toInt
    val m = items(2).drop("m=\"".length).dropRight("\"".length).toInt

    m match {
      case value: Int if (value & ChowFlag) > 0 => parseChow(value, who)
      case value: Int if (value & PongFlag) > 0 => parsePong(value, who)
      case value: Int if (value & KakanFlag) > 0 => parseKakan(value, who)
      case value: Int if (value & AnkanFlag) == 0 => parseAnkan(value, who)
      case value: Int if (value >> KitaBits & 1) == 1 => parseKita(value, who)
      case _ => parseMinKan(m, who)
    }

    DefaultReward
  }

  def parseGameEndMsg(msg: String): Int = {
    val iam = innerState.iam
    val items = extractMsg(msg).split(" ").map(_.trim)
    var reward: Int = DefaultReward

    msg match {
      case m: String if m.contains("AGARI") =>
        items.foreach(item => {
          if (item.startsWith("sc")) {
            reward = item.drop("sc=\"".length).dropRight("\"".length).split(",")(iam * 2 + 1).toInt
          }
          if (item.startsWith("who")) {
            val who = item.drop("who=\"".length).dropRight("\"".length).toInt
            if (who == iam) {
              val tile = (for (i <- items.indices if items(i).contains("machi")) yield items(i).drop("machi=\"".length).dropRight("\"".length).toInt).head
              MessageParseUtilsImpl.acceptTile(innerState, tile)
            }
          }
        })
      case m: String if m.contains("RYUUKYOKU") =>
        for (i <- items.indices) {
          if (items(i).startsWith("sc")) {
            reward = items(i).drop("sc=\"".length).dropRight("\"".length).split(",").map(_.trim).apply(1).toInt
          }
        }
    }

    reward
  }

}
