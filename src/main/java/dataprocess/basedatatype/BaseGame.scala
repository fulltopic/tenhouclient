package dataprocess.basedatatype

trait BaseGame {
  def addScene(scene: BaseScene): Unit
  def createScene(): BaseScene
  def getScenes(): List[BaseScene]

  def setLevels(levels: Array[Int]);
  def setRates(rates: Array[Int])
  def setFixOya(oya: Int)

  def getStateDim: List[Int]
}
