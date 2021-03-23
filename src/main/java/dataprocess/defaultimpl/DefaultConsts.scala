package dataprocess.defaultimpl

import dataprocess.basedatatype.BaseConsts

object DefaultConsts {
  //Layout
  val MeIndex: Int = BaseConsts.MeIndex
  val ChiPlayerIndex: Int = (MeIndex + BaseConsts.PlayerNum - 1) % BaseConsts.PlayerNum
  val PlayerNum: Int = BaseConsts.PlayerNum
  val NumPerTile: Int = BaseConsts.NumPerTile
  val TileNum: Int = BaseConsts.TileNum

  val Width = 34
  val Height = 4

  //Channels
  val MyCloseTile = 0
  val MyOpenTile = 1
  val MyDropTile = 2
  val Player1OpenTile: Int  = MyDropTile + 1
  val Player1DropTile: Int  = Player1OpenTile + 1
  val Player2OpenTile: Int  = Player1DropTile + 1
  val Player2DropTile: Int = Player2OpenTile + 1
  val Player3OpenTile: Int  = Player2DropTile + 1
  val Player3DropTile: Int  = Player3OpenTile + 1

  val DoraCh: Int  = Player3DropTile + 1
  val LevelRate: Int = DoraCh + 1
  val ScoreCh: Int = LevelRate + 1

  val ChiCh: Int = ScoreCh + 1
  val PonCh: Int = ChiCh + 1
  val KanCh: Int = PonCh + 1
  val ReachCh: Int = KanCh + 1

  val OyaReachFuriten: Int = ReachCh + 1

  val ChanNum: Int = OyaReachFuriten + 1

  val RateUnit = 150
  val LowRate = 600
  val HighRate = 2500
  val RateOffset = 18
  val LevelOffset = 0

  val ScoreUnit = 20
  val LowScore = 0
  val HighScore = 640

  val OyaOffset = 8
  val ReachOffset = 16
  val FuritenOffset = 24

  val ReachIndOffSet = 16

  //Actions
  val ReachAction: Int = TileNum * NumPerTile //136
  val ChiPos1Action: Int = ReachAction + 1 //137
  val ChiPos2Action: Int = ChiPos1Action + 1 //138
  val ChiPos3Action: Int = ChiPos2Action + 1 //139
  val PonMiss1Action: Int = ChiPos3Action + 1 //140
  val PonMiss2Action: Int = PonMiss1Action + 1 //141
  val PonMiss3Action: Int = PonMiss2Action + 1 //142
  val PonMiss4Action: Int = PonMiss3Action + 1 //143
  val KanAction: Int = PonMiss4Action + 1 //144
  val RonAction: Int = KanAction + 1 //145
  val NoAction: Int = RonAction + 1 //146
  val ActionNum: Int = NoAction + 1 //147
}
