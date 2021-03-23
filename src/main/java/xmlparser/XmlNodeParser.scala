package xmlparser

import dataprocess.basedatatype.BaseConsts

import scala.xml.{Node, Text, XML}
import org.slf4j.{Logger, LoggerFactory}

object XmlNodeParser {
  val logger: Logger = LoggerFactory.getLogger(getClass)

  val dropLabels: Set[Char] = Set[Char]('E', 'F', 'G', 'e', 'f', 'g')
  val drawLabels: Set[Char] = Set[Char]('U', 'V', 'W', 'u', 'v', 'w')
  val myDropLabels: Set[Char] = Set[Char]('D', 'd')
  val myDrawLabels: Set[Char] = Set[Char]('T', 't')
  val tileLabels: Set[Char] = dropLabels.union(drawLabels).union(myDrawLabels).union(myDropLabels)

  val flag2Player: Map[Char, Int] = Map[Char, Int]('T' -> 0, 'D' -> 0, 'U' -> 1, 'E' -> 1, 'V' -> 2, 'F' -> 2, 'W' -> 3, 'G' -> 3,
                                                  't' -> 0, 'd' -> 0, 'u' -> 1, 'e' -> 1, 'v' -> 2, 'f' -> 2, 'w' -> 3, 'g' -> 3)


  def getIntArrayFromAttribute(node: Node, attrName: String): Array[Int] = {
    val danAttr = node.attribute(attrName)
    danAttr match {
      case Some(x) => {
        x.map(_.text.split(",").map(_.toInt)).toArray.flatten
      }
      case _ => Array.emptyIntArray
    }
  }


  def getFloatIntArrayFromAttribute(node: Node, attrName: String): Array[Int] = {
    val danAttr = node.attribute(attrName)
    danAttr match {
      case Some(x) => {
        x.map(_.text.split(",").map(_.toFloat).map(_.toInt)).toArray.flatten
      }
      case _ => Array.emptyIntArray
    }
  }

  def getDanFromUN(node: Node): Array[Int] = {
    val dans = getIntArrayFromAttribute(node, "dan")
    logger.debug("Dans: {}", dans)

    dans
  }

  def getRateFromUN(node: Node): Array[Int] = {
    val rates = getFloatIntArrayFromAttribute(node, "rate")
    logger.debug("Rates: {}", rates)

    rates
  }

  def getTens(node: Node): Array[Int] = {
    val tens = getIntArrayFromAttribute(node, "ten")
    logger.debug("Tens: {}", tens)

    tens
  }

  def getIntFromAttr(node: Node, attrName: String): Int = {
    node.attribute(attrName).head.text.toInt
  }

  def isTileLabel(flag: Char): Boolean = {
    tileLabels.contains(flag)
  }

  def isDrawLabel(flag: Char): Boolean = {
    drawLabels.contains(flag)
  }

  def isDropLabel(flag: Char): Boolean = {
    dropLabels.contains(flag)
  }

  def isMyDrop(flag: Char): Boolean = myDropLabels.contains(flag)
  def isMyDraw(flag: Char): Boolean = myDrawLabels.contains(flag)

  def getRawFromLabel(label: String): Int = {
    label.substring(1).toInt
  }

  def getFlagFromLabel(label: String): Char = label.charAt(0)

  def getPlayerFromFlag(flag: Char): Int = flag2Player.getOrElse(flag, -1)

  def isChiM(m: Int): Boolean = (m & (1 << 2)) > 0
  def isPonM(m: Int): Boolean = (m & (1 << 3)) > 0
  def isKaKan(m: Int): Boolean = (m & (1 << 4)) > 0

  //https://github.com/NegativeMjark/tenhou-log
  def parseChiM(m: Int): (Int, Int, Array[Int]) = {
    var data = m

    val fromWho = data & 3

    data = data >> 3
    val t0 = data & 3
    data = data >> 2
    val t1 = data & 3
    data = data >> 2
    val t2 = data & 3

    data = data >> 3
    val called = data % 3

    val x = data / 3
    val remain = x % 7
    val multi = x / 7
    val base = multi * 9 + remain

    val tiles: Array[Int] = Array[Int](base * BaseConsts.NumPerTile + t0, (base + 1) * BaseConsts.NumPerTile + t1, (base + 2) * BaseConsts.NumPerTile + t2)
    (fromWho, tiles(called), tiles)
  }

  def parsePonM(m: Int): (Int, Int, Int, Array[Int]) = {
    var data = m

    val fromWho = data & 3

    data = data >> 5
    val t4 = data & 3

    data = data >> 4
    val called = data % 3
    val base = data / 3

    val tiles: Array[Int] = Array.ofDim[Int](3)
    var j: Int = 0
    for (i <- 0 until BaseConsts.NumPerTile) {
      if (i != t4) {
        tiles(j) = base * BaseConsts.NumPerTile + i
        j = j + 1
      }
    }

    (fromWho, tiles(called), t4, tiles)
  }

  def parseKakan(m: Int): (Int, Int, Array[Int]) = {
    var data = m

    val fromWho = data & 3

    data = data >> 5
    val t4 = data & 3

    data = data >> 4
    val called = data % 3
    val base = data / 3

    val tiles: Array[Int] = Array.ofDim[Int](4)
    for (i <- 0 until BaseConsts.NumPerTile) {
      tiles(i) = base * BaseConsts.NumPerTile + i
    }

    (fromWho, tiles(t4), tiles)
  }

  def parseKan(m: Int): (Int, Int, Array[Int]) = {
    var data = m

    val fromWho = data & 3

    data = data >> 8

    val called = data % 4
    val base = data / 4

    val tiles: Array[Int] = Array.ofDim[Int](4)
    for (i <- 0 until BaseConsts.NumPerTile) {
      tiles(i) = base * BaseConsts.NumPerTile + i
    }

    (fromWho, tiles(called), tiles)
  }
}
