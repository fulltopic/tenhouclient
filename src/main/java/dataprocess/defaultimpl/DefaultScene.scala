package dataprocess.defaultimpl

import dataprocess.basedatatype.{BaseScene, BaseTransaction}
import org.slf4j.{Logger, LoggerFactory}

class DefaultScene(levels: Array[Int], rates: Array[Int]) extends BaseScene{
  protected val logger: Logger = LoggerFactory.getLogger(getClass)

  protected var trans = List.empty[BaseTransaction];
  protected val curState = new DefaultState()
  protected var hangState: Option[DefaultState] = None
  protected var isReached: Boolean = false

  {
    for (i <- levels.indices) {
      curState.setLevel(i, levels(i))
      curState.setRate(i, rates(i))
    }
  }

  override def addTran(tran: BaseTransaction): Unit = {
    trans = trans :+ tran
  }

  override def getTrans(): List[BaseTransaction] = trans

  override def createTran(): BaseTransaction = {
    new DefaultTransaction(curState.clone().asInstanceOf[DefaultState])
  }

  private def createTran(state: DefaultState, reward: Int): DefaultTransaction = {
    val tran = new DefaultTransaction(state)
    tran.setReward(reward)
    addTran(tran)

    tran
  }
  private def copyState(): DefaultState = {
    curState.clone().asInstanceOf[DefaultState]
  }

  override def setOya(who: Int): Unit = curState.setOya(who)

  override def setLevel(who: Int, level: Int): Unit = curState.setLevel(who, level)

  override def setRate(who: Int, level: Int): Unit = curState.setRate(who, level)

  override def setScore(who: Int, score: Int): Unit = curState.setScore(who, score)

  override def setDora(raw: Int): Unit = curState.setDora(raw)

  def canAnkan(raw: Int): Boolean = {
    if (raw == 73) {
      logger.debug("Ankan: {}", curState.data(DefaultConsts.MyOpenTile))
      logger.debug("Ankan: {}", curState.data(DefaultConsts.MyCloseTile))
    }
    val closeNum = DefaultImplUtils.getClosTileNum(curState, raw)
    val openNum = DefaultImplUtils.getOpenTileNum(curState, raw)
//    logger.debug("Ankan detect {}: {}, {}", raw, closeNum, openNum)
    (closeNum == 3) || (openNum == 3)
  }

  def canKakan: (Boolean, List[Int]) = { //May have multi candidates
    var raws = List.empty[Int]
    for (i <- 0 until DefaultConsts.TileNum) {
//      if (i == 19) {
//        val closeTile = DefaultImplUtils.getClosTileNum(curState, i * 4)
//        val openTile = DefaultImplUtils.getOpenTileNum(curState, i * 4)
//        logger.debug("Check kankan: {}, {}", closeTile, openTile)
//      }
      if ((DefaultImplUtils.getClosTileNum(curState, i * 4) == 1) && (DefaultImplUtils.getOpenTileNum(curState, i * 4) == 3)) {
        var raw = i * 4
        for (j <- 0 until DefaultConsts.NumPerTile) {
          if (curState.data(DefaultConsts.MyCloseTile)(j)(i) > 0) {
            raw = i * 4 + j
          }
        }
        raws = raws :+ raw
//        return (true, raw)
      }

      if (DefaultImplUtils.getClosTileNum(curState, i * 4) == DefaultConsts.NumPerTile) {
        raws = raws :+ (i * 4 + 2)
//        return (true, i * 4 + 2) //Seemed log said so
      }
    }

    if (raws.isEmpty) (false, raws)
    else (true, raws)
//    (false, -1)
  }
//  def checkAddKan(raw: Int): Boolean = {
////    canAnkan(raw) || canKakan
//    if (canAnkan(raw)) {
////      logger.debug("Can ankan")
//      true
//    }else if (canKakan) {
////      logger.debug("Can kakan")
//      true
//    }else {
//      false
//    }
//  }


  override def addTile(raw: Int): Unit = {
    logger.debug("addTile {}", raw)
//    logger.debug("Before addTile: {}", DefaultImplUtils.getOpenTileNum(curState, raw))
    if (!isReached) {
      hangState match {
        case Some(state) => {
          logger.debug("Ignore previous indicator")
          val tran = createTran(state, 0)
          tran.setNoAction()

          hangState = None
        }
        case None => //nothing
      }

      if (curState.canReach(raw)) {
        val state = copyState()
        logger.debug("Can reach {}", raw)
//        logger.debug("Reach state: {}", state.data(DefaultConsts.MyCloseTile))
        state.setReachIndicator(raw)
        hangState = Some(state)
      }


      val (rc, rawTile) = canKakan
      if (rc) {
        logger.debug("Can kakan: {}", rawTile)
        hangState match {
          case Some(state) => rawTile.foreach(raw => state.setKanIndicator(raw))
          case None => {
            val state = copyState()
//            state.setKanIndicator(rawTile)
            rawTile.foreach(raw => state.setKanIndicator(raw))
            hangState = Some(state)
          }
        }
//        logger.debug("After set kan indicator: {}", hangState.get.data(DefaultConsts.KanCh))
      }
    } else {
      logger.debug("Reached")
    }

    if (canAnkan(raw)) {
      logger.debug("Can Ankan")
      hangState match {
        case Some(state) => state.setKanIndicator(raw)
        case None => {
          val state = copyState()
          state.setKanIndicator(raw)
          hangState = Some(state)
        }
      }
    }

    curState.addTile(raw)
  }

  override def rmTile(who: Int, raw: Int): Unit = {
    logger.debug("rmTile: {}, {}", who, raw)
    if (who == DefaultConsts.MeIndex) {
      if (!isReached) {
        hangState match {
          case Some(state) => { //Reach case
            logger.debug("Ignore reach by drop action")
            val tran = createTran(state, 0)
            tran.setDropAction(raw)

            hangState = None
          }
          case None => { //Normal drop
            logger.debug("Normal drop")
            val state = copyState()
            val tran = createTran(state, 0)
            tran.setDropAction(raw)
          }
        }
      } else {
        logger.debug("Reached")
      }
      curState.rmTile(who, raw)
    } else {
      hangState match {
        case Some(state) => {
          logger.debug("Ignore indicator by other's drop")
          val tran = createTran(state, 0)
          tran.setNoAction()

          hangState = None
        }
        case None => logger.debug("Other drop")
      }

      curState.rmTile(who, raw)
      if (!isReached) {
        val state = copyState()
        val setIndicator = setMeldIndicator(who, state, raw)
        if (setIndicator) {
          logger.debug("Indicator set by other's drop")
          hangState = Some(state)
        }
      }
    }
  }

  override def setReach(who: Int, step: Int): Unit = {
    logger.debug("setReach: {}, {}", who, step)
    if (who == DefaultConsts.MeIndex) {
      if (step == 2) {
        logger.debug("Me set reach step 2")
        isReached = true
      } else {
        hangState match {
          case Some(state) => {
            logger.debug("Me set reach step 1")
            val tran = createTran(state, 0) //TODO: tmp reach reward
            tran.setReachAction(step)

            hangState = None
          }
          case None => {
            logger.error("Unexpected, no hang state for reach action")
            logger.error("Reach state: {}", curState.data(DefaultConsts.MyCloseTile))
          }
        }
      }
      curState.setReach(who, step) //Only one drop action after reach action
    } else {
      hangState match {
        case Some(state) => {
          if (step == 1) {
            logger.debug("Other reach, no action")
            val tran = createTran(state, 0)
            tran.setNoAction()

            hangState = None
          } else {
            //There maybe indicator set in drop between reach step 1 and step 2
            logger.debug("step2 does not interrupt indicator")
          }
        }
        case None => //nothing
      }
      curState.setReach(who, step)
    }
  }

  override def setFuriten(who: Int): Unit = curState.setFuriten(who)

  override def rmFuriten(who: Int): Unit = curState.rmFuriten(who)


  def setMeldIndicator(fromWho: Int, state: DefaultState, rawCalled: Int): Boolean = {
    var setIndicator: Boolean = false

    if (fromWho == DefaultConsts.ChiPlayerIndex) {
      if (DefaultImplUtils.canChi(state, rawCalled)) {
        logger.debug("Set chi indicator {}") //, curState.data(DefaultConsts.MyCloseTile))
        state.setChiIndicator(rawCalled)
        setIndicator = true
      }
    }
    if (DefaultImplUtils.canPon(state, rawCalled)) {
      logger.debug("Set pon indicator {}") //, curState.data(DefaultConsts.MyCloseTile))
      state.setPonIndicator(rawCalled)
      setIndicator = true
    }
    if (DefaultImplUtils.canKakan(state, rawCalled)) {
      logger.debug("Set kakan indicator {}", curState.data(DefaultConsts.MyOpenTile))
      state.setKanIndicator(rawCalled)
      setIndicator = true
    }
    if (DefaultImplUtils.canKan(state, rawCalled)) {
      logger.debug("Set kan indicator {}", curState.data(DefaultConsts.MyCloseTile))
      state.setKanIndicator(rawCalled)
      setIndicator = true
    }

//    if (setIndicator) {
//      val (rc, rawTile) = canKakan
//      if (rc) {
//        logger.debug("Set ankan indicator in rmTile: {}", rawTile)
//        state.setKanIndicator(rawTile)
//      }
//    }

    setIndicator
  }

  override def setChi(who: Int, fromWho: Int, rawTiles: Array[Int], rawCalled: Int): Unit = {
    logger.debug("setChi: {}, {}, {}", who, fromWho, rawCalled)
//    logger.debug("curState: {}", curState.data(DefaultConsts.MyCloseTile))
    if (who == DefaultConsts.MeIndex) {
      hangState match {
        case Some(state) => {
          logger.debug("Me chi")
          var chiPos: Int = 0
          for (i <- rawTiles.indices) {
            if (rawTiles(i) == rawCalled) {
              chiPos = i
            }
          }
          val action = DefaultConsts.ChiPos1Action + chiPos
          val tran = createTran(state, 0)
          tran.setChiAction(action)

          hangState = None

          for (i <- 0 until DefaultConsts.Height; j <- 0 until DefaultConsts.Width) {
            if (state.data(DefaultConsts.ChiCh)(i)(j) > 0) {
              if (i != rawCalled % DefaultConsts.NumPerTile) {
                logger.error("Not match hangState and called {} != {}, {}", rawCalled, i, j)
              }
              if (j != rawCalled / DefaultConsts.NumPerTile) {
                logger.error("Not match hangState and called {} != {}, {}", rawCalled, i, j)
              }
            } else if ((j * 4 + i) == rawCalled) {
              logger.error("No indicator set {}: {}, {}", rawCalled, i, j)
            }
          }
        }
        case None => logger.error("Unexpected: chi without hang state")
      }
    } else {
      hangState match {
        case Some(state) => {
          logger.debug("hang state interrupted by other")
          val tran = createTran(state, 0)
          tran.setNoAction()

          hangState = None
        }
        case None => //nothing
      }
    }

    curState.setChi(who, fromWho, rawTiles, rawCalled)
  }

  override def setPon(who: Int, fromWho: Int, rawCalled: Int, rawTile4: Int): Unit = {
    logger.debug("setPon: {}, {}, {}, {}", who, fromWho, rawCalled, rawTile4)
    if (who == DefaultConsts.MeIndex) {
      hangState match {
        case Some(state) => {
          logger.debug("Me pon")
          val action = DefaultConsts.PonMiss1Action + rawTile4
          val tran = createTran(state, 0)
          tran.setPonAction(action)

          hangState = None

          for (i <- 0 until DefaultConsts.Height; j <- 0 until DefaultConsts.Width) {
            if (state.data(DefaultConsts.PonCh)(i)(j) > 0) {
              if (i != rawCalled % DefaultConsts.NumPerTile) {
                logger.error("Not match hangState and called {} != {}, {}", rawCalled, i, j)
              }
              if (j != rawCalled / DefaultConsts.NumPerTile) {
                logger.error("Not match hangState and called {} != {}, {}", rawCalled, i, j)
              }
            } else if ((j * 4 + i) == rawCalled) {
              logger.error("No indicator set {}: {}, {}", rawCalled, i, j)
            }
          }
        }
        case None => logger.error("Unexpected: pon without hang state")
      }
    } else {
      hangState match {
        case Some(state) => {
          logger.debug("hang state interrupted by other")
          val tran = createTran(state, 0)
          tran.setNoAction()

          hangState = None
        }
        case None => //nothing
      }
    }

//    logger.debug("Before pon: {}", DefaultImplUtils.getClosTileNum(curState, rawCalled))
    curState.setPon(who, fromWho, rawCalled, rawTile4)
//    logger.debug("After pon: {}", DefaultImplUtils.getOpenTileNum(curState, rawCalled))
  }

  private def setAllKan(who: Int, fromWho: Int, rawCalled: Int): Unit = {
    if (who == DefaultConsts.MeIndex) {
      hangState match {
        case Some(state) => {
          logger.debug("Me kan")
          val tran = createTran(state, 0)
          tran.setKanAction()

          hangState = None

          val indNum = DefaultImplUtils.getTileNum(state, rawCalled, DefaultConsts.KanCh)
          if (indNum <= 0) {
            logger.error("Kan no indicator set {} from {}", rawCalled, fromWho)
            logger.error("Close tiles: {}", state.data(DefaultConsts.MyCloseTile))
            logger.error("Open tiles: {}", state.data(DefaultConsts.MyOpenTile))
            logger.error("Kan indicator: {}", state.data(DefaultConsts.KanCh))
          }

//          for (i <- 0 until DefaultConsts.Height; j <- 0 until DefaultConsts.Width) {
//            if (state.data(DefaultConsts.KanCh)(i)(j) > 0) {
//              if (i != rawCalled % DefaultConsts.NumPerTile) {
//                logger.error("Not match hangState and called {} != {}, {}", rawCalled, i, j)
//              }
//              if (j != rawCalled / DefaultConsts.NumPerTile) {
//                logger.error("Not match hangState and called {} != {}, {}", rawCalled, i, j)
//              }
//            } else if ((j * 4 + i) == rawCalled) {
//              logger.error("No indicator set {} from {}: {}, {}", rawCalled, fromWho, i, j)
//              logger.error("Close tiles: {}", state.data(DefaultConsts.MyCloseTile))
//              logger.error("Open tiles: {}", state.data(DefaultConsts.MyOpenTile))
//              logger.error("Kan indicator: {}", state.data(DefaultConsts.KanCh))
//            }
//          }
        }
        case None => {
          logger.error("Unexpected: kan without hang state: {}", curState.data(DefaultConsts.MyCloseTile))
          logger.error("Unexpected: kan without hang state: {}", curState.data(DefaultConsts.MyOpenTile))
        }
      }
    } else {
      hangState match {
        case Some(state) => {
          logger.debug("hang state interrupted by other")
          val tran = createTran(state, 0)
          tran.setNoAction()

          hangState = None
        }
        case None => //nothing
      }
    }
  }

  override def setChaKan(who: Int, fromWho: Int, rawCalled: Int): Unit = {
    logger.debug("setChaKan: {}, {}, {}", who, fromWho, rawCalled)
    setAllKan(who, fromWho, rawCalled)
    curState.setChaKan(who, fromWho, rawCalled)
  }

  override def setKan(who: Int, fromWho: Int, rawCalled: Int): Unit = {
    logger.debug("setKan: {}, {}, {}", who, fromWho, rawCalled)
    setAllKan(who, fromWho, rawCalled)
    curState.setKan(who, fromWho, rawCalled)
  }

  override def getDims: List[Int] = curState.getDims()

  //TODOED: Separate noaction and scene end action
  override def setAgari(who: Int, fromWho: Int, reward: Int, tiles: Array[Int]): Unit = {
    logger.debug("setAgari: {} from {} with {}", who, fromWho, reward)
    if (who == DefaultConsts.MeIndex) {
      logger.debug("I won")
      //hangState would have been removed by drop or draw
      val state = copyState() //Impossible for hanging state
      val tran = createTran(state, reward)
      tran.setRonAction()
    } else {
      if (fromWho == DefaultConsts.MeIndex) {
        //hangState would have been removed by previous draw
        logger.debug("Wrong drop")
        val tran = trans.last
        logger.debug("Last action: {}", tran.getAction)
        tran.setReward(reward)
        if (DefaultImplUtils.isDropAction(tran.getAction)) {
          logger.info("Last tran is drop action")
        } else if (DefaultImplUtils.isReachAction(tran.getAction)) {
          logger.info("Last tran is reach action")
        } else if (DefaultImplUtils.isKanAction(tran.getAction)) {
          logger.info("Last tran is kan action")
        } else {
          logger.warn("Unexpecte: Last tran is not drop action")
        }
      } else {
        hangState match {
          case Some(state) => {
            logger.debug("Indicator ignored by agari")
            val tran = createTran(state, reward)
            tran.setNoAction()
          }
          case None => logger.debug("nothing")

        }
        val state = copyState()
        val tran = createTran(state, reward)
        tran.setNoAction()
      }
    }
  }

  override def setRyu(reward: Int, tiles: Array[Int]): Unit = {
    logger.debug("setRyu: {}", reward)
    hangState match {
      case Some(state) => {
        logger.debug("Indicator interrupted by tiles")
        val tran = createTran(state, reward)
        tran.setNoAction()
        hangState = None
      }
      case None => //nothing
    }

    logger.debug("Ryu end transaction")
    val tran = createTran(copyState(), reward)
    tran.setNoAction()
  }
}
