package dataprocess

import dataprocess.GameFactory.GameType.GameType
import dataprocess.basedatatype.BaseGame
import dataprocess.defaultimpl.DefaultGame

object GameFactory {
   object GameType extends Enumeration {
     type GameType = Int
     val Default = 1
   }

  def GetGame(gameType: GameType): BaseGame = {
    gameType match {
      case _ => new DefaultGame
    }
  }
}
