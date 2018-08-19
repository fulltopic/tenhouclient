package tenhouclient.impl

import tenhouclient.utils.TenhouConsts._

object ImplConsts {
  val PeerStateLen: Int = TileNum + TileNum + 1 * PlayerNum + 1 + 1 //mine, on board, reach, myreach, oya
  val PeerOyaIndex: Int = PeerStateLen - 1
  val PeerReachIndex: Int = PeerOyaIndex - 1
  val PeerCommonReach: Int = TileNum * 2

  val ReachReward: Int = -10
  val DefaultReward = 0


  val Accept: Int = TileNum
  val Chow: Int = Accept + 1
  val Pong: Int = Chow + 1
  val KaKan: Int = Pong + 1
  val AnKan: Int = KaKan + 1
  val MinKan: Int = AnKan + 1
  val REACH: Int = MinKan + 1
  val Ron: Int = REACH + 1
  val NOOP: Int = Ron + 1
  val ActionLen: Int = NOOP + 1



  val ChowWoAccept: Int = TileNum  //34
  val PongWoAccept: Int = ChowWoAccept + 1 //35
  val KaKanWoAccept: Int = PongWoAccept + 1 //36
  val AnKanWoAccept: Int = KaKanWoAccept + 1 //37
  val MinKanWoAccept: Int = AnKanWoAccept + 1 //38
  val REACHWoAccept: Int = MinKanWoAccept + 1 //39
  val RonWoAccept: Int = REACHWoAccept + 1 //40
  val NOOPWoAccept: Int = RonWoAccept + 1 //41
  val ActionLenWoAccept: Int = NOOPWoAccept + 1 //42

  val DoraValue = 8
  val MValue = 64 //fixed
  val ExtraValueFlag = 7

  val AkaValues = Map(16 -> DoraValue, 52 -> DoraValue, 88 -> DoraValue)

  val kanValue: Double = 0.1
  val chowValue: Double = 0.25
  val pongValue: Double = 0.5
  val reachValue: Double = 0.5

  val ActionStep0: Int = 0
  val ActionStep1: Int = 1
  val ActionStep2: Int = 2
  val InvalidAction: Int = -1
  val InvalidTile: Int = -1
}
