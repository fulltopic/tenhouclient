package tenhouclient.utils

import akka.event.slf4j.Logger
import tenhouclient.utils.TenhouConsts._

object MessageParseUtils{
  private val logger = Logger("MessageParseUtils")

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


  val StartConnection = "StartConnection"
  val CloseConnection = "CloseConnection"
  val ClosedConnection = "ClosedConnection"
  val SendGameEndReply = "SendGameEndReply"
  //  val AbortedConnection = "AbortedConnection"
  val ResetAction: Int = -1

  def isSceneKey(msg: String): Boolean = {
    //
    if (isGameMsg(msg)) true
    else sceneKeys.foldLeft[Boolean](false)((a, key) => { a || msg.contains(key) })
  }

  def isGameMsg(msg: String): Boolean = {
    val key = getKeyInMsg(msg)
    if (gameMsgSet.contains(key)) true
    else key.matches(dropKeyPattern) || key.matches(acceptKeyPattern) || key.matches(myAcceptPattern) || key.matches(myDropPattern)
  }

  def isReachIndicator(action: Int): Boolean = action == 32

  def isRonIndicator(action: Int): Boolean = {
    ronActionFlags.contains(action)
  }


  def extractMsg(msg: String): String = {
    msg.trim.drop(1).dropRight(2).trim
  }


  def getTileTile(msg: String): Int = {
    msg.drop(1).toInt
  }

  def getLogMsg(s: String): String = {
    //    "<%s />".format(s) + "\0\0"
    "<" + s + "/>\0"
  }

  private[this] val translation_table = Array[Int](63006, 9570, 49216, 45888, 9822, 23121, 59830, 51114, 54831, 4189, 580, 5203, 42174, 59972,
    55457, 59009, 59347, 64456, 8673, 52710, 49975, 2006, 62677, 3463, 17754, 5357)


  def getHeloMsg(userName: String): String = {
    getLogMsg("HELO name=\"" + userName + "\" tid=\"f0\" sx=\"M\" ")
    //    "<HELO name=\"" + userName + "\" tid=\"f0\" sx=\"M\" />\0\0"
  }

  def getHeloReply(heloMsg: String): String = {
    val items = heloMsg.split(" ").map(_.trim)
    val authMsg = items.filter(m => m.contains("auth"))(0).drop("auth=\"".length).dropRight("\"".length)
    val parts = authMsg.split("-")
    val part1 = parts(0)
    val part2 = parts(1)

    val tableIndex = ("2" + part1.drop(2)).toInt % (12 - part1.takeRight(1).toInt) * 2
    val a = translation_table(tableIndex) ^ BigInt(part2.take(4), 16).toInt
    val b = translation_table(tableIndex + 1) ^ BigInt(part2.takeRight(4), 16).toInt
    val postfix = ("%02X".format(a) + "%02X".format(b)).toLowerCase()

    getLogMsg("AUTH val=\"" + part1 + "-" + postfix + "\"") //TODO: What's this format?
  }

  def getPxrMsg(): String = {
    getLogMsg("PXR V=\"9\"")
  }

  def getKAMsg(): String = {
    getLogMsg("Z ")
  }

  def getJoinMsg(): String = {
    getLogMsg("JOIN t=\"0,1\"")
  }

  def getRejoinMsg(msg: String): String = {
    val joinMsg = msg.trim.drop(7).dropRight(2).trim
    getLogMsg("JOIN " + joinMsg)
  }

  def getGokMsg(): String = {
    getLogMsg("GOK")
  }

  def getNextReadyMsg(): String = {
    getLogMsg("NEXTREADY")
  }

  def getByeMsg(): String = getLogMsg("BYE ")

  def getIam(msg: String): Int = {
    val items = msg.split("<")
    val iamItem = items(2)
    if (iamItem.contains("TAIKYOKU")) {
      iamItem.drop(14).take(1).toInt
    }else {
      logger.debug("Can not get TAIKYOKU tag")
      0
    }
  }

  def isTerminalMsg(msg: String): Boolean = {
    msg.contains("AGARI") || msg.contains("RYUUKYOKU")
  }





  def isReachAction(action: Int): Boolean = {
    action == ReachStep1 || action == ReachStep2
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


  def getWhoFromN(msg: String): Int = {
    val content = extractMsg(msg)
    content.split(" ").map(_.trim).apply(1).drop("who=\"".length).dropRight("\"".length).toInt
  }


  def getKeyInMsg(msg: String): String = {
    msg.trim.drop(1).dropRight(2).trim.split(" ").head
  }

  def getMsgItems(msg: String): Seq[String] = {
    extractMsg(msg).split(" ").map(_.trim)
  }

  def dropQuotation(msg: String, title: String): String = {
    msg.drop((title + "=\"").length).dropRight("\"".length)
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
      val items = extractMsg(msg).split(" ").map(_.trim)
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

  def getRonIndicator(msg: String): Int = {
    extractMsg(msg).split(" ").map(_.trim).apply(1).drop("t=\"".length).dropRight("\"".length).toInt
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


  def dropTileFromMsg(msg: String): Int = {
    val content = MessageParseUtils.extractMsg(msg)
    content.split(" ").map(_.trim).apply(1).drop("hai=\"".length).dropRight("\"".length).toInt
  }

  def isMyReachIndicator(msg: String): Boolean = {
    //    logger.debug("Check if my reach indicator " + msg)
    var isStep1: Boolean = false

    if (msg.contains("REACH")) {
      val items = extractMsg(msg).split(" ").map(_.trim)

      if (items.length == 3) {
        val whoStr = items(1)
        val stepStr = items(2)

        val who = whoStr.drop("who=\"".length).dropRight("\"".length).toInt
        val step = stepStr.drop("step=\"".length).dropRight("\"".length).toInt

        if (who == 0 && step == 1) {
          isStep1 = true
        }
      }
    }

    isStep1
  }

}

