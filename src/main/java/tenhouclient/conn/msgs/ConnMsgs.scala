package tenhouclient.conn.msgs

object ConnMsgs {
  val StartConnection = "StartConnection"
  val StartConnectionRsp = "SentStartConnection"
  val CloseConnection = "CloseConnection"
  val ClosedConnection = "ClosedConnection"
  val SendGameEndReply = "SendGameEndReply"

  val ResetAction: Int = -1
}
