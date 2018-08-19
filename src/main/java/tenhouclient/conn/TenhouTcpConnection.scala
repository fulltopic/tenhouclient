package tenhouclient.conn

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef}
import akka.event.slf4j.Logger
import akka.io.Tcp._
import akka.io.{IO, Tcp}
import akka.util.ByteString
import tenhouclient.conn.msgs.{ConnMsgs, ConnReady}
import tenhouclient.config.ClientSettings
import tenhouclient.utils.MessageParseUtils

class TenhouTcpConnection(remote: InetSocketAddress, listener: ActorRef, val index: Int = 0) extends Actor{
  private[this] val log = Logger("TenhouTcpConnection" + index)

  val kaMsg = "<Z />"
  var kaNum: Int = 0
  val kaLimit: Int = ClientSettings.KALimit


  def receive = {
    case msg: String if msg.contains(ConnMsgs.StartConnection) =>
      Thread.sleep(1000) //So tenhouclient.client can start immediately
      import context.system
      IO(Tcp) ! Connect(remote, None, List[akka.io.Inet.SocketOption](SO.TcpNoDelay(false)))

      log.info("Tcp connecting " + remote)
      context.become(started(sender()))
    case x: AnyRef =>
      log.error("Received unexpected message " + x)
  }

  def started(connection: ActorRef): Receive = {
    case CommandFailed(_: Connect) =>
      listener ! "connection failed"
      context stop self

    case c @ Connected(remote, local) =>
      log.info("Connection connected from " + local + " to " + remote)
      val connection = sender()
      connection ! Register (self)
      log.info("tenhouclient.conn information " + connection)

      listener ! new ConnReady
      //      listener ! "YES"

      context.become(connected(connection))
  }

  def connected(connection : ActorRef): Receive = {
    case ConnMsgs.CloseConnection =>
      log.error("tcp connection closing, received close command")
      connection ! Close
    case s: String =>
      connection ! Write(ByteString(s))
      log.info("sent to server " + s.replace("\0", " "))
      if (s.replace("\0", " ").trim.equals(kaMsg)) {
        kaNum += 1

        if (kaNum > kaLimit) {
          log.error("Failed to receive message from server, to abort current connection")
          self ! ConnMsgs.CloseConnection
        }
      }
    case CommandFailed(w: Write) =>
      listener ! "Write failed"
    case Received(data) =>
      kaNum = 0

      val rawMsg = data.utf8String
      val msgs = rawMsg.split("\0")
      msgs.foreach(msg => processData(msg))
    case x: ConnectionClosed =>
      log.error("" + x.getClass())
      log.error("tcp connection aborted")
      listener ! ConnMsgs.ClosedConnection
      context.become(receive)
    case x: Any =>
      log.error("Received unknown command " + x)
  }


  def processData(msg: String): Unit = {
    msg match {
      case s if MessageParseUtils.isSceneKey(s) => listener ! s
      case _ => log.error("Received unrecoganizable message " + msg)
    }
  }
}
