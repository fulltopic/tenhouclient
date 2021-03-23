package dbprocess

import caffe2.caffe2.{TensorProto, TensorProtos}
import dataprocess.GameFactory
import dataprocess.GameFactory.GameType
import dataprocess.GameFactory.GameType.GameType
import dataprocess.basedatatype.{BaseGame, BaseScene}
import org.lmdbjava.{Dbi, DbiFlags, Env, Txn}
import org.slf4j.{Logger, LoggerFactory}
import xmlparser.XmlFileProcessor

import java.io.File
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import scala.annotation.tailrec

class LmdbGenerator (dbPath: String, xmlDirPath: String, gameType: GameType) {
  val logger: Logger = LoggerFactory.getLogger(getClass)
  //generate dir file for file iterator
  //generate db
  //process file, 4 dims
  val dbFile = new File(dbPath)
  dbFile.deleteOnExit()
  val env: Env[ByteBuffer] = Env.create().setMapSize((1024 * 1024) * 1024L * 2).setMaxDbs(1).open(dbFile)
  val db: Dbi[ByteBuffer] = env.openDbi("TEST", DbiFlags.MDB_CREATE)
  val dummyGame: BaseGame = GameFactory.GetGame(gameType)
  val stateDims: List[Long] = dummyGame.getStateDim.map(_.toLong)

  val keyBuffer: ByteBuffer = ByteBuffer.allocateDirect(env.getMaxKeySize)
  val valueBuffer: ByteBuffer = ByteBuffer.allocateDirect((stateDims.product * 20 * 2).toInt)

  def process(): Unit = {
    val files = getFileList
    files.foreach(processFile)
//    processFile(files.head)

    db.close()
  }

  private def getFileList: List[String] = {
    val dir = new File(xmlDirPath)
    dir.listFiles().filter(file => file.isFile).map(_.getAbsolutePath).toList
  }

  private def processFile(filePath: String): Unit = {
    logger.info("Process file: {}", filePath)
//    logger.debug("File name: {}", filePath.split("/").last)
//    logger.debug("Last part: {}", filePath.split("/").last.split("-").last)
//    logger.debug("Target: {}", filePath.split("/").last.split("-").last.split("&").head)
    val keyBase = filePath.split("/").last.split("-").last.split("&").head

    val game = GameFactory.GetGame(gameType)
    val parser = new XmlFileProcessor(filePath, game)
    parser.readFile()

    processScene(keyBase, 0, game.getScenes())
//    var seq: Int = 0
//    game.getScenes().foreach(scene => {
//      processScene(keyBase, seq, scene)
//      seq = seq + 1
//    })
  }

  @tailrec
  private def processScene(keyBase: String, seq: Int, scenes: List[BaseScene]): Unit = {
    logger.info("Process scene {}", seq)
    scenes match {
      case Nil => //nothing
      case scene :: tail => {
        var datas = List.empty[Array[Int]]
        var actions = List.empty[Int]
        var rewards = List.empty[Float]

        //TODO: Check data size
        scene.getTrans().foreach(tran => {
          datas = datas :+ tran.getState.getData()
          actions = actions :+ tran.getAction
          rewards = rewards :+ tran.getReward
        })

        //TODO: Check dims
        val dims: List[Long] = datas.size.toLong :: stateDims
        val dataValue = datas.flatten

        val dataProto = TensorProto().withDims(dims).withDataType(TensorProto.DataType.INT32).addAllInt32Data(dataValue)
        val actionProto = TensorProto().withDims(List[Long](actions.size)).withDataType(TensorProto.DataType.INT32).addAllInt32Data(actions)
        val rewardProto = TensorProto().withDims(List[Long](rewards.size)).withDataType(TensorProto.DataType.INT32).addAllFloatData(rewards)
        val tranProtos = TensorProtos().withProtos(List[TensorProto](dataProto, actionProto, rewardProto))

        val keyValue = keyBase + "-" + seq.toString

//        val txn = env.txnWrite()
        keyBuffer.clear()
        valueBuffer.clear()
        keyBuffer.put(keyValue.getBytes(StandardCharsets.UTF_8)).flip()
        valueBuffer.put(tranProtos.toByteArray).flip()
        db.put(keyBuffer, valueBuffer)
//        txn.commit()
//        txn.close()

        processScene(keyBase, seq + 1, tail)
      }
    }
  }
}
