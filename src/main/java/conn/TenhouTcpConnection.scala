package conn

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef}
import akka.event.slf4j.Logger
import akka.io.Tcp._
import akka.io.{IO, Tcp}
import akka.util.ByteString
import config.ClientSettings
import conn.msgs.{ConnMsgs, ConnReady}
import utils.MessageParseUtils

class TenhouTcpConnection(remote: InetSocketAddress, listener: ActorRef, val index: Int = 0) extends Actor{
//  val log = Logging(context.system, this)
  private[this] val log = Logger("TenhouTcpConnection" + index)

  val kaMsg = "<Z />"
  var kaNum: Int = 0
  val kaLimit: Int = ClientSettings.KALimit

//  var connection: ActorRef = null

  def receive = {
    case msg: String if msg.contains(ConnMsgs.StartConnection) =>
      Thread.sleep(1000) //So client can start immediately
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
      log.info("conn information " + connection)

      listener ! new ConnReady
      //      listener ! "YES"

      context.become(connected(connection))
  }

  def connected(connection : ActorRef): Receive = {
    case ConnMsgs.CloseConnection =>
      log.error("tcp connection closing, received close command")
      //      println("tcp connection closing")
      //      System.exit(0)
      connection ! Close
    //      context.become(myClose(connection))
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
    //          val msg = data.utf8String.replace("\0", " ")
    //          log.info("Received msg " + msg)
    //          processData(msg)
    case x: ConnectionClosed =>
      log.error("" + x.getClass())
      log.error("tcp connection aborted")
      listener ! ConnMsgs.ClosedConnection
      context.become(receive)
    case x: Any =>
      log.error("Received unknown command " + x)
  }


  def processData(msg: String): Unit = {
//    log.debug("Received data from server " + msg)
    msg match {
      case s if MessageParseUtils.isSceneKey(s) => listener ! s
      case _ => log.error("Received unrecoganizable message " + msg)
    }
  }
}
