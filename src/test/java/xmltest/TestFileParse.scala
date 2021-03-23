package xmltest

import dataprocess.defaultimpl.{DefaultGame, DefaultImplUtils}
import org.slf4j.{Logger, LoggerFactory}
import xmlparser.XmlFileProcessor

import java.io.File

object TestFileParse extends App {
  val logger: Logger = LoggerFactory.getLogger(getClass)



  def checkRemain(): Unit = {
    val remains = List[Int](5)
    val toReach = DefaultImplUtils.checkRemains(remains, false)
    println(toReach)
  }

  def checkRegular0() = {
    val tiles = Array[Int]( 0, 2, 0, 1, 1, 1, 0, 0, 0, 0, 0, 3, 2, 2, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    val remains = List.empty[Int]
    val toReach = DefaultImplUtils.checkRegular(tiles, remains, false, 0)
    println("------------> " + toReach)
  }

  def checkRegular1() = {
    val tiles = Array[Int]( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    val remains = List[Int](11)
    val toReach = DefaultImplUtils.checkRegular(tiles, remains, true, 14)
    println("------------> " + toReach)
  }

  def checkLastRemain() = {
    val remains = List[Int](11, 15, 16)
    val toReach = DefaultImplUtils.checkLastRemain(remains, true)
    println("toReach: " + toReach)
  }

  def checkRegular2() = {
    val tiles = Array[Int](0, 0, 3, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 2, 1, 1, 0, 0, 0, 0, 0, 2, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0)
    val remains = List.empty[Int]
    val toReach = DefaultImplUtils.checkRegular(tiles, remains, false, 0)
    println("------------> " + toReach)
  }

  def checkRegular3() = {
    val tiles = Array[Int](0, 0, 0, 0, 0, 1, 0, 2, 0, 0,    0, 0, 0, 0, 0, 0, 0, 0, 0, 0,    0, 3, 1, 1, 1, 1, 1, 0, 0, 4,   0, 0, 0, 0)
    val remains = List.empty[Int]
    val toReach = DefaultImplUtils.checkRegular(tiles, remains, false, 0)
    println("------------> " + toReach)
  }

  def getFileList(dirPath: String): List[File] = {
    val dFile = new File(dirPath)
    if (dFile.exists() && dFile.isDirectory) {
      dFile.listFiles().filter(_.isFile).toList
    } else {
      List.empty[File]
    }
  }

  def checkFile(): Unit = {
    LoggerFactory.getILoggerFactory match {
      case loggerContext: ch.qos.logback.classic.LoggerContext => loggerContext.getLoggerList.forEach(_.setLevel(ch.qos.logback.classic.Level.DEBUG))
      case _ => logger.error("Not logback backend, unable to set log level")
    }

    //    val fileName = "/run/media/zf/Newsmy/mjres/pf4/test/test5.xml"
    val fileName = "/run/media/zf/Newsmy/mjres/pf4/xmls/xml_pf4-20_n4/2009090722gm-00e1-0000-af7cbbb4&tw=2.xml"
    val game = new DefaultGame
    val parser = new XmlFileProcessor(fileName, game)

    parser.readFile()
  }

  def processDir(): Unit = {
    LoggerFactory.getILoggerFactory match {
      case loggerContext: ch.qos.logback.classic.LoggerContext => loggerContext.getLoggerList.forEach(_.setLevel(ch.qos.logback.classic.Level.INFO))
      case _ => logger.error("Not logback backend, unable to set log level")
    }

//    val dirPath = "/run/media/zf/Newsmy/mjres/pf4/test/xmls/xml_pf4-20_n1"
    val dirPath = "/run/media/zf/Newsmy/mjres/pf4/xmls/xml_pf4-20_n6"
    val files = getFileList(dirPath)
    var games = List.empty[DefaultGame]
    files.foreach(file => {
      logger.info("Get file: {}", file.getAbsolutePath)
      val game = new DefaultGame
//      games = games :+ game
      val processor = new XmlFileProcessor(file.getAbsolutePath, game)
      processor.readFile()
    })

  }

//  checkLastRemain()
//  checkRegular0()
//  checkRegular1()
//  checkRegular2()
//  checkRegular3()
  checkFile()
//  processDir()
}
