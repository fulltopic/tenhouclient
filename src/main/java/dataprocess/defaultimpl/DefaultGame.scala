package dataprocess.defaultimpl

import dataprocess.basedatatype.{BaseConsts, BaseGame, BaseScene}

class DefaultGame extends BaseGame{
  protected var scenes = List.empty[BaseScene]
  protected val levels: Array[Int] = Array.ofDim(BaseConsts.PlayerNum)
  protected val rates: Array[Int] = Array.ofDim(BaseConsts.PlayerNum)
  protected var fixOya: Int = 0 //Currently ignored

  override def addScene(scene: BaseScene): Unit = {
    scenes = scenes :+ scene
  }

  override def createScene(): BaseScene = {
    val scene = new DefaultScene(levels, rates)
    addScene(scene)

    scene
  }

  override def getScenes(): List[BaseScene] = scenes

  override def setLevels(levels: Array[Int]): Unit = {
    //TODO: Assert size match
    for (i <- levels.indices) {
      this.levels(i) = levels(i)
    }
  }

  override def setRates(rates: Array[Int]): Unit = {
    for (i <- rates.indices) {
      this.rates(i) = rates(i)
    }
  }

  override def setFixOya(oya: Int): Unit = {
    this.fixOya = oya
  }

  override def getStateDim: List[Int] = {
    DefaultState.GetDims()
  }
}
