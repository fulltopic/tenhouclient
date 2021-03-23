package dataprocess.defaultimpl

import dataprocess.basedatatype.{BaseState, BaseTransaction}

class DefaultTransaction(state: DefaultState) extends BaseTransaction {
//  protected val state: DefaultState = new DefaultState
  protected var action: Int = 0
  protected var reward: Float = 0

  override def getState: BaseState = state

  override def getAction: Int = action

  override def getReward: Float = reward

//  override def setAction(action: Int): Unit = {
//    this.action = action
//  }

  override def setReward(reward: Float): Unit = {
    if (reward > 0) {
      this.reward = 1
    } else if (reward < 0) {
      this.reward = -1
    }
  }

  override def setDropAction(raw: Int): Unit = {
    action = raw
  }

  override def setReachAction(step: Int): Unit = {
    if (step == 1) {
      action = DefaultConsts.ReachAction
    }
  }

  override def setRonAction(): Unit = {
    action = DefaultConsts.RonAction
  }

  override def setNoAction(): Unit = {
    action = DefaultConsts.NoAction
  }

  //Meld from 0
  override def setChiAction(meldPos: Int): Unit = {
    action = DefaultConsts.ChiPos1Action + meldPos
  }

  //tile4Pos from 0
  override def setPonAction(tile4Pos: Int): Unit = {
    action = DefaultConsts.PonMiss1Action + tile4Pos
  }

  override def setKanAction(): Unit = {
    action = DefaultConsts.KanAction
  }

  override def setChiIndicator(raw: Int): Unit = state.setChiIndicator(raw)

  override def setPonIndicator(raw: Int): Unit = state.setPonIndicator(raw)

  override def setChakanIndicator(raw: Int): Unit = state.setChakanIndicator(raw)

  override def setKanIndicator(raw: Int): Unit = state.setKanIndicator(raw)

  override def setReachIndicator(raw: Int): Unit = state.setReachIndicator(raw)

  override def setRonIndicator(who: Int, ronType: Int): Unit = state.setRonIndicator(who, ronType)
}

