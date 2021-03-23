package dataprocess.basedatatype

trait BaseState {
  def setOya(who: Int): Unit;
  def setLevel(who: Int, level: Int): Unit;
  def setRate(who: Int, level: Int): Unit;
  def setScore(who: Int, score: Int): Unit;

  def setDora(raw: Int): Unit;

  def addTile(raw: Int): Unit;
  def rmTile(who: Int, raw: Int): Unit;

  def setReach(who: Int, step: Int): Unit;
  def setFuriten(who: Int): Unit;
  def rmFuriten(who: Int): Unit;

  def setChiIndicator(raw: Int): Unit;
  def setPonIndicator(raw: Int): Unit;
  def setChakanIndicator(raw: Int): Unit;
  def setKanIndicator(raw: Int): Unit;
  def setReachIndicator(raw: Int): Unit;
  def setRonIndicator(who: Int, ronType: Int): Unit;

  def setChi(who: Int, fromWho: Int, rawTiles: Array[Int], rawCalled: Int): Unit;
  def setPon(who: Int, fromWho: Int, rawCalled: Int, rawTile4: Int): Unit;
  def setChaKan(who: Int, fromWho: Int, rawCalled: Int): Unit; //from pon to kan 杠
  def setKan(who: Int, fromWho: Int, rawCalled: Int): Unit;

  def getDims(): List[Int];

  def getData(): Array[Int]
}
