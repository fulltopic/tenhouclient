package tenhouclient.config

object ClientSettings {
  val KALimit: Int = 5 //Reconnection after #KALimit of KA received while no message sent from client

  val KASnap: Int = 15 // #KASnap seconds sleep between two KA messages sent

  //After AGARI or PROF received, decision would be made after at least #GameEndWaitTime seconds
  //As there is no fixed sequence of these messages and no explicit message to indicate end of game of end of epoch
  val GameEndWaitTime: Int = 3

  // It is implementation dependent. Which number represents no operation or reach
  val NOOPWoAccept: Int = 41

  val REACHWoAccept: Int = 39
}
