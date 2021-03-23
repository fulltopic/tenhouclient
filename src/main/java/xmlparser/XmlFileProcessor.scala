package xmlparser

import dataprocess.basedatatype.{BaseConsts, BaseGame, BaseScene, BaseTransaction}

import scala.io.Source
import scala.xml.{Node, Text, XML}

import org.slf4j.{Logger, LoggerFactory}

class XmlFileProcessor(fileName: String, game: BaseGame) {
  val logger: Logger = LoggerFactory.getLogger(getClass)
  val fixDoras: Set[Int] = Set[Int](16, 52, 88)

  def readFile(): Unit = {
    val content = Source.fromFile(fileName).mkString
    val root = XML.loadString(content)

    val unNode = root \ "UN"
    readUN(unNode.head, game)

    val taikyokuNode = root \ "TAIKYOKU"
    readTAIKYOKU(taikyokuNode.head, game)

    val scenes = root \ "SCENE"
    scenes.foreach(scene => {
      val sceneRecord = game.createScene()

      fixDoras.foreach(dora => sceneRecord.setDora(dora))

      readScene(scene, sceneRecord)
    })
  }

  def readUN(node: Node, game: BaseGame): Unit = {
    val levels = XmlNodeParser.getDanFromUN(node)
    val rates = XmlNodeParser.getRateFromUN(node)

    game.setLevels(levels)
    game.setRates(rates)
  }

  def readTAIKYOKU(node: Node, game: BaseGame): Unit = {
    val oya = node.attribute("oya").head.text.toInt
    game.setFixOya(oya)
  }

  def readScene(sceneNode: Node, sceneRecord: BaseScene): Unit = {
    sceneNode.child.foreach {
      case _: Text => //nothing
      case node@(_: Node) => {
//        val tran = sceneRecord.createTran()
        readNode(node, sceneRecord)
      }
    }
  }

  def readNode(node: Node, scene: BaseScene): Unit = {
//    logger.debug("Parse node {}", node.label)
    node.label match {
      case "INIT" => readInit(node, scene)
      case "N" => readN(node, scene)
      case "REACH" => readReach(node, scene)
      case "DORA" => readDora(node, scene)
      case "FURITEN" => readFuriten(node, scene)
      case "AGARI" => readAgari(node, scene)
      case "RYUUKYOKU" => readRYUUKYOKU(node, scene)
      case _ => readTiles(node, scene)
    }
  }

  def readInit(node: Node, scene: BaseScene): Unit = {
    logger.debug("readInit")
    //TODO: There may be dora info in init message
    val tens = XmlNodeParser.getTens(node)
    val hais = XmlNodeParser.getIntArrayFromAttribute(node, "hai0")
    val oya = XmlNodeParser.getIntFromAttr(node, "oya")

    for (i <- tens.indices) {
      scene.setScore(i, tens(i))
    }
    hais.foreach(raw => scene.addTile(raw))

    scene.setOya(oya)
  }

  def readN(node: Node, scene: BaseScene): Unit = {
    val who = XmlNodeParser.getIntFromAttr(node, "who")
    val m = XmlNodeParser.getIntFromAttr(node, "m")

    m match {
      case m: Int if XmlNodeParser.isChiM(m) => readChiNode(who, m, scene)
      case m: Int if XmlNodeParser.isPonM(m) => readPonNode(who, m, scene)
      case m: Int if XmlNodeParser.isKaKan(m) => readKakanNode(who, m, scene)
      case _ => readKanNode(who, m, scene)
    }
  }

  def readChiNode(who: Int, m: Int, scene: BaseScene): Unit = {
    val (fromWho, called, tiles) = XmlNodeParser.parseChiM(m)
    scene.setChi(who, fromWho, tiles, called)
  }

  def readPonNode(who: Int, m: Int, scene: BaseScene): Unit = {
    val (fromWho, called, t4, _) = XmlNodeParser.parsePonM(m)
    scene.setPon(who, fromWho, called, t4)
  }

  def readKakanNode(who: Int, m: Int, scene: BaseScene): Unit = {
    val (fromWho, called, _) = XmlNodeParser.parseKakan(m)
    scene.setKan(who, fromWho, called)
  }

  def readKanNode(who: Int, m: Int, scene: BaseScene): Unit = {
    val (fromWho, called, _) = XmlNodeParser.parseKan(m)
    scene.setKan(who, fromWho, called)
  }

  def readReach(node: Node, scene: BaseScene): Unit = {
    val who = XmlNodeParser.getIntFromAttr(node, "who")
    val step = XmlNodeParser.getIntFromAttr(node, "step")

     scene.setReach(who, step)
  }

  def readDora(node: Node, scene: BaseScene): Unit = {
    logger.debug("readDora")
    val dora = XmlNodeParser.getIntFromAttr(node, "hai")
    scene.setDora(dora)
  }

  //TODOED: Check other's furiten. Not found in xml logs
  def readFuriten(node: Node, scene: BaseScene): Unit = {
    val show = XmlNodeParser.getIntFromAttr(node, "show")
    if (show == 1) {
      scene.setFuriten(BaseConsts.MeIndex)
    } else {
      scene.rmFuriten(BaseConsts.MeIndex)
    }
  }

  def readAgari(node: Node, scene: BaseScene): Unit = {
    val scores = XmlNodeParser.getIntArrayFromAttribute(node, "sc")
    val score = scores(1)
    val who = XmlNodeParser.getIntFromAttr(node, "who")
    val fromWho = XmlNodeParser.getIntFromAttr(node, "fromWho")
    val tiles = XmlNodeParser.getIntArrayFromAttribute(node, "hai")

    scene.setAgari(who, fromWho, score, tiles)
  }

  def readRYUUKYOKU(node: Node, scene: BaseScene): Unit = {
    val scores = XmlNodeParser.getIntArrayFromAttribute(node, "sc")
    val score = scores(1)
    scene.setRyu(score, Array.empty[Int])
  }

  //TODOED: rm indicator function of scene so curState reusable
  def readTiles(node: Node, scene: BaseScene): Unit = {
    val flag = XmlNodeParser.getFlagFromLabel(node.label)
    flag match {
      case f: Char if XmlNodeParser.myDrawLabels(f) => readMyDraw(node, scene)
      case f: Char if XmlNodeParser.myDropLabels(f) => readMyDrop(node, scene)
      case f: Char if XmlNodeParser.dropLabels(f) => readOtherDrop(node, scene)
      case f: Char if XmlNodeParser.drawLabels(f) => //nothing
      case _ => //nothing
    }
  }

  def readOtherDrop(node: Node, scene: BaseScene): Unit = {
    val flag = XmlNodeParser.getFlagFromLabel(node.label)
    val raw = XmlNodeParser.getRawFromLabel(node.label)
    val who = XmlNodeParser.getPlayerFromFlag(flag)

    //TODOED: Check action indicator
    scene.rmTile(who, raw)
  }

  //TODOED: Check reach indicator
  def readMyDraw(node: Node, scene: BaseScene): Unit = {
    val raw = XmlNodeParser.getRawFromLabel(node.label)
    scene.addTile(raw)
  }

  //TODOED: Check giveup indicator
  def readMyDrop(node: Node, scene: BaseScene): Unit = {
    val raw = XmlNodeParser.getRawFromLabel(node.label)
    scene.rmTile(BaseConsts.MeIndex, raw)
  }
}
