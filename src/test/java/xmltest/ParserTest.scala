package xmltest

import dataprocess.defaultimpl.{DefaultConsts, DefaultGame, DefaultState}
import org.slf4j.{Logger, LoggerFactory}
import xmlparser._

object ParserTest extends App {
  val logger: Logger = LoggerFactory.getLogger(getClass)

  val fileName = "/run/media/zf/Newsmy/mjres/pf4/test/xmls/xml_pf4-20_n1/2009062410gm-0089-0000-bf3a1dc5&tw=1.xml"
  val game = new DefaultGame()
  val processor = new XmlFileProcessor(fileName, game)

  processor.readFile()

//  val scene = game.createScene()
//  val tran = scene.createTran()
//  val state = tran.getState.asInstanceOf[DefaultState]

  def headInfo(): Unit = {
    val scenes = game.getScenes()
    val tran = scenes.head.createTran()
    val state = tran.getState.asInstanceOf[DefaultState]
    val headData = state.data(DefaultConsts.LevelRate)
    logger.debug("Head info in state: {}", headData)
  }

  def initInfo(): Unit = {
    val state = game.getScenes().head.createTran().getState.asInstanceOf[DefaultState]
    val initData = state.data(DefaultConsts.ScoreCh)
    logger.debug("Tens: {}", initData)

    val hais = state.data(DefaultConsts.MyCloseTile)
    logger.debug("Hai0: {}", hais)

    val oya = state.data(DefaultConsts.OyaReachFuriten)
    logger.debug("Init oya: {}", oya)
  }

  def doraInfo(): Unit = {
    val scenes = game.getScenes()
    scenes.foreach(scene => {
      val state = scene.getTrans().last.getState.asInstanceOf[DefaultState]
      val dora = state.data(DefaultConsts.DoraCh)

      logger.info("Dora: {}", dora)
    })
  }

  def parseM(m: Int): Unit = {
    m match {
      case m: Int if XmlNodeParser.isChiM(m) => {
        logger.info("Chi meld")
        val (fromWho, tile, tiles) = XmlNodeParser.parseChiM(m)
        logger.info("meld {} from {} into {}", tile, fromWho, tiles)
      }
      case m: Int if XmlNodeParser.isPonM(m) => {
        logger.info("Pon meld")
        val (fromWho, tile, t4, tiles) = XmlNodeParser.parsePonM(m)
        logger.info("meld {} from {} into {}", tile, fromWho, t4, tiles)
      }
      case m: Int if XmlNodeParser.isKaKan(m) => {
        logger.info("Kakn meld")
        val (fromWho, tile, tiles) = XmlNodeParser.parseKakan(m)
        logger.info("meld {} from {} into {}", tile, fromWho, tiles)
      }
      case _ => {
        logger.info("Kan meld")
        val (fromWho, tile, tiles) = XmlNodeParser.parseKan(m)
        logger.info("meld {} from {} into {}", tile, fromWho, tiles)
      }
    }
  }

  def testM(): Unit = {
    parseM(56679) //chi, 95, (88, 95, 98)
    parseM(50794) //pon, 132, (132, 133, 134)
    parseM(25681) //kakan, 66, (64, 65, 66, 67)
    parseM(24065) //kan, (92, 93, 94, 95)
    parseM(30257)
  }

//  headInfo()
//  initInfo()
//  doraInfo()
  testM()
}

