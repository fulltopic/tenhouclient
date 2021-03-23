package dbtest

import dataprocess.defaultimpl.DefaultConsts
import dbtest.TestLmdbGen.getClass
import org.slf4j.{Logger, LoggerFactory}

object TestReadState {
  val SeqIndex: Int = 0
  val ChIndex: Int = 1
  val HIndex: Int = 2
  val WIndex: Int = 3

  val logger: Logger = LoggerFactory.getLogger(getClass)

  def parseTiles(chanId: Int, seq: Int, data: List[Int], dims: Array[Long]): List[Int] = {
    val recordSize = dims.tail.product
    var startIndex = seq * recordSize
    startIndex += chanId * dims(HIndex) * dims(WIndex)
    data.slice(startIndex.toInt, startIndex.toInt + (dims(HIndex) * dims(WIndex)).toInt)
  }

  def parseOpenTiles(seq: Int, data: List[Int], dims: Array[Long]): Unit = {
    val tiles = parseTiles(DefaultConsts.MyOpenTile, seq, data, dims)

    logger.info("Open tiles: {}", tiles)
  }

  def parseCloseTiles(seq: Int, data: List[Int], dims: Array[Long]): Unit = {
    val tiles = parseTiles(DefaultConsts.MyCloseTile, seq, data, dims)

    logger.info("Close tiles: {}", tiles)
  }
}
