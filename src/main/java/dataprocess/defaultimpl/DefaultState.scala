package dataprocess.defaultimpl

import dataprocess.basedatatype._

class DefaultState() extends BaseState with Cloneable {
  val data: Array[Array[Array[Int]]] = Array.ofDim[Int](DefaultConsts.ChanNum, DefaultConsts.Height, DefaultConsts.Width)

  def setOya(who: Int): Unit = {
    data(DefaultConsts.OyaReachFuriten)(who)(DefaultConsts.OyaOffset) = 1
  }

  override def setLevel(who: Int, level: Int): Unit = {
    data(DefaultConsts.LevelRate)(who)(level) = 1
  }

  override def setRate(who: Int, rate: Int): Unit = {
    val rateIndex = DefaultImplUtils.getRateIndex(rate)
    data(DefaultConsts.LevelRate)(who)(rateIndex) = 1
  }

  override def setScore(who: Int, score: Int): Unit = {
    val scoreIndex = DefaultImplUtils.getScoreIndex(score)
    data(DefaultConsts.ScoreCh)(who)(scoreIndex) = 1
  }

  override def setDora(raw: Int): Unit = {
    val index = DefaultImplUtils.getStatePos(raw)
    data(DefaultConsts.DoraCh)(index._1)(index._2) = 1
  }

  override def addTile(raw: Int): Unit = {
    val index = DefaultImplUtils.getStatePos(raw)
    data(DefaultConsts.MyCloseTile)(index._1)(index._2) = 1
  }

  override def rmTile(who: Int, raw: Int): Unit = {
    val index = DefaultImplUtils.getStatePos(raw)
    val chIndex =
      who match {
        case 0 => DefaultConsts.MyDropTile
        case 1 => DefaultConsts.Player1DropTile
        case 2 => DefaultConsts.Player2DropTile
        case 3 => DefaultConsts.Player3DropTile
        case _ => -1
      }

    if (who == DefaultConsts.MeIndex) {
      data(DefaultConsts.MyCloseTile)(index._1)(index._2) = 0
    }
    data(chIndex)(index._1)(index._2) = 1
  }

  override def setReach(who: Int, step: Int): Unit = {
    data(DefaultConsts.OyaReachFuriten)(who)(DefaultConsts.ReachOffset) = 1
  }

  override def setFuriten(who: Int): Unit = {
    data(DefaultConsts.OyaReachFuriten)(who)(DefaultConsts.FuritenOffset) = 1
  }

  override def rmFuriten(who: Int): Unit = {
    data(DefaultConsts.OyaReachFuriten)(who)(DefaultConsts.FuritenOffset) = 0
  }

  override def setChiIndicator(raw: Int): Unit = {
    val index = DefaultImplUtils.getStatePos(raw)
    data(DefaultConsts.ChiCh)(index._1)(index._2) = 1
  }

  override def setPonIndicator(raw: Int): Unit = {
    val index = DefaultImplUtils.getStatePos(raw)
    data(DefaultConsts.PonCh)(index._1)(index._2) = 1
  }

  override def setChakanIndicator(raw: Int): Unit = {
    setKanIndicator(raw)
  }

  override def setKanIndicator(raw: Int): Unit = {
    val index = DefaultImplUtils.getStatePos(raw)
    data(DefaultConsts.KanCh)(index._1)(index._2) = 1
  }

  override def setReachIndicator(raw: Int): Unit = {
    val index = DefaultImplUtils.getStatePos(raw)
    data(DefaultConsts.ReachCh)(index._1)(index._2) = 1
  }

  override def setRonIndicator(who: Int, ronType: Int): Unit = {
    //Always ron immediately
  }

  def set3Meld(who: Int, rawTiles: Array[Int], rawCalled: Int): Unit ={
    if (who == DefaultConsts.MeIndex) {
      rawTiles.foreach(raw => {
        if (raw != rawCalled) {
          val index = DefaultImplUtils.getStatePos(raw)
          data(DefaultConsts.MyCloseTile)(index._1)(index._2) = 0
        }
      })
    }

    val whoIndex = DefaultImplUtils.getOpenChanIndex(who)
    rawTiles.foreach(raw => {
      val index = DefaultImplUtils.getStatePos(raw)
      data(whoIndex)(index._1)(index._2) = 1
    })
  }

  override def setChi(who: Int, fromWho: Int, rawTiles: Array[Int], rawCalled: Int): Unit = {
    set3Meld(who, rawTiles, rawCalled)
  }

  override def setPon(who: Int, fromWho: Int, rawCalled: Int, rawTile4: Int): Unit = {
    val tile = DefaultImplUtils.raw2Tile(rawCalled)
    val whoIndex = DefaultImplUtils.getOpenChanIndex(who)

    for (i <- 0 until DefaultConsts.NumPerTile) {
      val candidate = i + tile * DefaultConsts.NumPerTile
      if (who == DefaultConsts.MeIndex) {
        if (candidate != rawCalled && i != rawTile4) {
          data(DefaultConsts.MyCloseTile)(i)(tile) = 0
        }
      }
      if (i != rawTile4) {
        data(whoIndex)(i)(tile) = 1
      }
    }
  }

  override def setChaKan(who: Int, fromWho: Int, rawCalled: Int): Unit = {
    val whoIndex = DefaultImplUtils.getOpenChanIndex(who)
    val index = DefaultImplUtils.getStatePos(rawCalled)
    data(whoIndex)(index._1)(index._2) = 1
  }

  //TODOED: Ankan
  //TODO: indicate kan even if tiles are in closeTiles
  override def setKan(who: Int, fromWho: Int, rawCalled: Int): Unit = {
    val (callIndex, tile) = DefaultImplUtils.getStatePos(rawCalled)

    if ((who == DefaultConsts.MeIndex) && (fromWho == DefaultConsts.MeIndex) && (DefaultImplUtils.getClosTileNum(this, rawCalled) >= 3)) {
      //Ankan
      for (i <- 0 until DefaultConsts.NumPerTile) {
        data(DefaultConsts.MyCloseTile)(i)(tile) = 1
      }
    } else {
      val chanIndex = DefaultImplUtils.getOpenChanIndex(who)
      for (i <- 0 until DefaultConsts.NumPerTile) {
        if (who == DefaultConsts.MeIndex) {
          data(DefaultConsts.MyCloseTile)(i)(tile) = 0
        }
        data(chanIndex)(callIndex)(tile) = 1
      }
    }
  }

  def canReach(raw: Int): Boolean = {
    if (data(DefaultConsts.OyaReachFuriten)(DefaultConsts.MeIndex)(DefaultConsts.ReachOffset) == 1) return false

    val openTiles = data(DefaultConsts.MyOpenTile)
    var sum: Int = 0
    for (i <- openTiles.indices; j <- openTiles(0).indices) {
      sum += openTiles(i)(j)
    }
    if (sum > 0) return false

    checkReach(raw)
  }

  def checkReach(raw: Int): Boolean = {
    val closeTiles = data(DefaultConsts.MyCloseTile)
    val tiles: Array[Int] = Array.ofDim[Int](DefaultConsts.Width)
    var sum: Int = 0
    for (i <- closeTiles.indices; j <- closeTiles(0).indices) {
      tiles(j) += closeTiles(i)(j)
      sum += closeTiles(i)(j)
    }
    tiles(raw / DefaultConsts.NumPerTile) += 1
    sum += 1

    if (sum < 14) return false

    DefaultImplUtils.checkReach(tiles)
  }


  override def getDims(): List[Int] = DefaultState.GetDims()

  override def clone(): AnyRef = {
    val copyState = new DefaultState
    for (i <- data.indices; j <- data(0).indices; k <- data(0)(0).indices) {
      copyState.data(i)(j)(k) = data(i)(j)(k)
    }

    copyState
  }

  override def getData(): Array[Int] = {
    data.flatten.flatten
  }
}

object DefaultState {
  def GetDims(): List[Int] = List[Int](DefaultConsts.ChanNum, DefaultConsts.Height, DefaultConsts.Width)
}
