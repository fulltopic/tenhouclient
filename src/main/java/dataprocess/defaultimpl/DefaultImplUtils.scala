package dataprocess.defaultimpl

import org.slf4j.{Logger, LoggerFactory}

object DefaultImplUtils {
  val logger: Logger = LoggerFactory.getLogger(getClass)

  def getStatePos(raw: Int): (Int, Int) = {
    val tile = raw / DefaultConsts.NumPerTile
    val seq = raw % DefaultConsts.NumPerTile

    (seq, tile)
  }

  def getRateIndex(rate: Int): Int = {
    logger.debug("setRate: {}", rate)
    if (rate < DefaultConsts.LowRate) {
      DefaultConsts.RateOffset
    } else if (rate > DefaultConsts.HighRate) {
      DefaultConsts.Width - 1
    } else {
      (rate - DefaultConsts.LowRate) / DefaultConsts.RateUnit + DefaultConsts.RateOffset
    }
  }

  def getScoreIndex(score: Int): Int = {
    if (score > DefaultConsts.HighScore) {
      DefaultConsts.Width - 1
    } else if (score < 0) {
      0
    } else {
      score / DefaultConsts.ScoreUnit
    }
  }

  def getOpenChanIndex(who: Int): Int = {
    1 + who * 2
  }

  def getDropChanIndex(who: Int): Int = {
    2 + who * 2
  }

  def raw2Tile(raw: Int): Int = {
    raw / DefaultConsts.NumPerTile
  }

  def getTileNum(state: DefaultState, raw: Int, chanIndex: Int): Int = {
    val tile = raw2Tile(raw)
    if (tile < 0 || tile >= DefaultConsts.TileNum) {
      0
    } else {
      var sum: Int = 0
      (0 until DefaultConsts.NumPerTile).foreach(i => {
        sum += state.data(chanIndex)(i)(tile)
      })

      sum
    }
  }

  def getClosTileNum(state: DefaultState, raw: Int): Int = {
    getTileNum(state, raw, DefaultConsts.MyCloseTile)
  }

  def getOpenTileNum(state: DefaultState, raw: Int): Int = {
    getTileNum(state, raw, DefaultConsts.MyOpenTile)
  }

  def canPon(state: DefaultState, called: Int): Boolean = {
    getClosTileNum(state, called) >= 2
  }

  val chiCandidates: List[(Int, Int)] = List[(Int, Int)]((-1, -2), (-1, 1), (1, 2))
  def canChi(state: DefaultState, called: Int): Boolean = {
    val tile = raw2Tile(called)

    def tileCandidate(c: Int): Boolean = {
      (c >= 0) && (c < 27)
    }

    def sameTileType(t1: Int, t2: Int): Boolean = (t1 / 9) == (t2 / 9)
    def chiCandidate(c: (Int, Int)): Boolean = {
      c match {
        case x: (Int, Int) if !tileCandidate(x._1 + tile) => false
        case x: (Int, Int) if !tileCandidate(x._2 + tile) => false
        case x: (Int, Int) if !sameTileType(tile, tile + x._1) => false
        case x: (Int, Int) if !sameTileType(tile, tile + x._2) => false
        case x: (Int, Int) if getClosTileNum(state, called + x._1 * DefaultConsts.NumPerTile) <= 0 => false
        case x: (Int, Int) if getClosTileNum(state, called + x._2 * DefaultConsts.NumPerTile) <= 0 => false
        case _ => true
      }
    }

    if (!tileCandidate(tile)) { return false }
    for (c <- chiCandidates if chiCandidate(c)) return true
    false
  }

  def canKakan(state: DefaultState, called: Int): Boolean = {
    getOpenTileNum(state, called) >= 2
  }

  def canKan(state: DefaultState, called: Int): Boolean = (getClosTileNum(state, called) >= 3)

  def check7Pairs(tiles: Array[Int]): Boolean = {
//    var singles: Int = 0
//    for (i <- tiles.indices if (tiles(i) % 2) > 0) singles += 1
//
//    singles == 2
    //kan not 2 pairs
    var pairs: Int = 0
    for (i <- tiles.indices if (tiles(i) >= 2)) pairs += 1
    pairs >= 6
  }

  def mayChow(t0: Int, t1: Int, t2: Int): Boolean = {
    if (t0 >= 27 || t1 >= 27 || t2 >= 27) false
    else if ((t0 / 9 != t1 / 9) || (t1 / 9 != t2 / 9)) false
    else if ((t1 != t0 + 1) || (t2 != t1 + 1)) false
    else true
  }

  def mayChow(t0: Int, t1: Int): Boolean = {
    t0 match {
      case t: Int if t >= 27 => false
      case _ => (t0 / 9 == t1 / 9) && ((t1 - t0).abs <= 2)
    }
  }

  def checkLastRemain(remains: List[Int], hasPair: Boolean): Boolean = {
//    logger.debug("checkLastRemains {}: {}", hasPair, remains)
    def mayChi(t0: Int, t1: Int): Boolean = {
      if (t0 >= 27) false
      else if (t0 / 9 != t1 / 9) false
      else if ((t1 - t0).abs > 2) false
      true
    }

    remains.size match {
      case 0 => false
      case 1 => false
      case 2 => (!hasPair)
      case 3 => {
        val tiles = remains.toArray
        if ((tiles(0) == tiles(1)) || (tiles(1) == tiles(2))) true
        else if (mayChi(tiles(0), tiles(1)) || mayChi(tiles(1), tiles(2))) true
        else false
      }
      case _ => false
    }
  }

  def checkRemains(remains: List[Int], hasPair: Boolean): Boolean = {
//    logger.debug("checkRemains {}: {}", hasPair, remains)
    remains.size match {
      case 0 => true
      case 1 => true
      case 2 => true
//        (hasPair && ((remains.head == remains.tail.head) || mayChow(remains.head, remains.last))) || !hasPair
      case 3 =>
        ((remains.head == remains.tail.head) || (remains.tail.head == remains.last)) ||
           (mayChow(remains.head, remains.tail.head) || mayChow(remains.tail.head, remains.last))
      case _ => false
    }
  }

  def checkRegular(remainTiles: Array[Int], remains: List[Int], hasPair: Boolean, startIndex: Int): Boolean = {
//    logger.debug("check Regular {}, {}", hasPair, startIndex)
//    logger.debug("tiles: {}", remainTiles)
//    logger.debug("remains: {}", remains)
    if (startIndex >= remainTiles.length) return checkLastRemain(remains, hasPair)
    if (remainTiles(startIndex) <= 0) return checkRegular(remainTiles, remains, hasPair, startIndex + 1)

    if (remainTiles(startIndex) >= 4) {
      remainTiles(startIndex) -= 4
      if (checkRegular(remainTiles, remains, hasPair, startIndex)) return true
      remainTiles(startIndex) += 4
    }
    if (remainTiles(startIndex) >= 3) {
      remainTiles(startIndex) -= 3
      if (checkRegular(remainTiles, remains, hasPair, startIndex)) return true
      remainTiles(startIndex) += 3
    }
    if (!hasPair && remainTiles(startIndex) >= 2) {
      remainTiles(startIndex) -= 2
      if (checkRegular(remainTiles, remains, true, startIndex)) return true
      remainTiles(startIndex) += 2
    }

    if (startIndex <= 24) {
      if (remainTiles(startIndex) > 0 && remainTiles(startIndex + 1) > 0 && remainTiles(startIndex + 2) > 0) {
        if (mayChow(startIndex, startIndex + 1, startIndex + 2)) {
          remainTiles(startIndex) -= 1
          remainTiles(startIndex + 1) -= 1
          remainTiles(startIndex + 2) -= 1
          if (checkRegular(remainTiles, remains, hasPair, startIndex)) return true
          remainTiles(startIndex) += 1
          remainTiles(startIndex + 1) += 1
          remainTiles(startIndex + 2) += 1
        }
      }
    }

    val tileNum = remainTiles(startIndex)
    val trialNum = tileNum.min(if (hasPair) 2 else 1)


    for (i <- 0 until trialNum) {
      remainTiles(startIndex) -= 1
      val delta = List.fill[Int](i + 1)(startIndex)
//      val newRemains = remains :+ startIndex
      val newRemains = remains ++ delta
      if (checkRemains(newRemains, hasPair)) {
        if (checkRegular(remainTiles, newRemains, hasPair, startIndex)) return true
      }
    }

    remainTiles(startIndex) = tileNum
    false
  }

  def checkReach(tiles: Array[Int]): Boolean = {
//    if (tiles(28) > 0) logger.debug("CheckReach: {}", tiles)

    if (check7Pairs(tiles)) {
      logger.debug("Reach 7 pairs")
      return true
    }

    val remains = List.empty[Int]
    checkRegular(tiles, remains, false, 0)
  }

  def isDropAction(action: Int): Boolean = action < DefaultConsts.Width * DefaultConsts.Height

  def isReachAction(action: Int): Boolean = (action == DefaultConsts.ReachAction)

  def isKanAction(action: Int): Boolean = action == DefaultConsts.KanAction
}
