package dbtest

import caffe2.caffe2.TensorProtos
import dataprocess.GameFactory
import dataprocess.GameFactory.GameType
import dataprocess.defaultimpl.DefaultConsts
import dbprocess.LmdbGenerator
import org.lmdbjava.{DbiFlags, Env, SeekOp}
import org.slf4j.{Logger, LoggerFactory}
import prototest.TestCompile.dbPath
import sun.nio.cs.UTF_8
import xmltest.TestFileParse.logger

import java.io.File
import java.nio.ByteBuffer
import scala.annotation.tailrec

object TestLmdbGen extends App {
  val logger: Logger = LoggerFactory.getLogger(getClass)
  LoggerFactory.getILoggerFactory match {
    case loggerContext: ch.qos.logback.classic.LoggerContext => loggerContext.getLoggerList.forEach(_.setLevel(ch.qos.logback.classic.Level.INFO))
    case _ => logger.error("Not logback backend, unable to set log level")
  }

  val dbPath = "/run/media/zf/Newsmy/mjres/pf4/test/testdb"

  def testGen(): Unit = {
    val dirPath = "/run/media/zf/Newsmy/mjres/pf4/test/xmls/xml_pf4-20_n1"

    val gen = new LmdbGenerator(dbPath, dirPath, GameType.Default)
    gen.process()
  }

  def testRead(): Unit = {
    val dbFile = new File(dbPath)
    val env: Env[ByteBuffer] = Env.create().setMapSize((1024 * 1024) * 1024L * 2L).setMaxDbs(1).open(dbFile)
    val db = env.openDbi("TEST", DbiFlags.MDB_CREATE)

    val stateSize = GameFactory.GetGame(GameType.Default).getStateDim.product
//    val keyBuffer = Array.ofDim[Byte](env.getMaxKeySize)
//    val valueBuffer = Array.ofDim[Byte](stateSize * 20 * 2)

    val txn = env.txnRead()
    val cursor = db.openCursor(txn)
    cursor.seek(SeekOp.MDB_FIRST)

//    var printState: Boolean = true
    var sceneId: Int = 0
    var tranId: Int = 0

    @tailrec
    def readRecords(): Unit = {
      val key = cursor.key()
      val value = cursor.`val`()

      //    key.get(keyBuffer)
      val valueBuffer = Array.ofDim[Byte](value.remaining())
      value.get(valueBuffer)

      val keyStr = UTF_8.INSTANCE.decode(key)
      logger.info("Get key: {}", keyStr)

      val protos = TensorProtos.parseFrom(valueBuffer)
      logger.info("Get proto size: {}", protos.protos.size)

      protos.protos.foreach(proto => {
        logger.info("Get proto {}: {}", proto.dataType, proto.dims)
      })
      logger.info("Get actions: {}", protos.protos(1).int32Data.toArray)
      logger.info("Get rewards: {}", protos.protos(2).floatData.toArray)

      if (sceneId == 2) {
        TestReadState.parseCloseTiles((protos.protos.head.dims.head - 1).toInt, protos.protos.head.int32Data.toList, protos.protos.head.dims.toArray)
        TestReadState.parseOpenTiles((protos.protos.head.dims.head - 1).toInt, protos.protos.head.int32Data.toList, protos.protos.head.dims.toArray)
        val drops = TestReadState.parseTiles(DefaultConsts.Player2DropTile, (protos.protos.head.dims.head - 1).toInt, protos.protos.head.int32Data.toList, protos.protos.head.dims.toArray)
        logger.info("Drop from 2: {}", drops)
      }

      sceneId += 1

      if (cursor.next()) {
        readRecords()
      }
    }

    readRecords()

    db.close()
  }

  testGen()
//  testRead()
}
