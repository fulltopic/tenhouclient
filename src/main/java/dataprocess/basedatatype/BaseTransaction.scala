package dataprocess.basedatatype

trait BaseTransaction {
  def getState: BaseState;
  def getAction: Int;
  def getReward: Float;

//  def setAction(action: Int): Unit
  def setReward(reward: Float): Unit

  def setDropAction(raw: Int): Unit
  def setReachAction(step: Int): Unit
  def setRonAction(): Unit
  def setNoAction(): Unit
  def setChiAction(meldPos: Int): Unit
  def setPonAction(tile4Pos: Int): Unit
  def setKanAction(): Unit

  def setChiIndicator(raw: Int): Unit;
  def setPonIndicator(raw: Int): Unit;
  def setChakanIndicator(raw: Int): Unit;
  def setKanIndicator(raw: Int): Unit;
  def setReachIndicator(raw: Int): Unit;
  def setRonIndicator(who: Int, ronType: Int): Unit;
}
