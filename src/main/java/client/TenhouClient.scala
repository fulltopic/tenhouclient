package client

import java.net.InetSocketAddress
import java.util.concurrent.TimeUnit

import akka.actor.{Actor, ActorRef, Props}
import akka.event.slf4j.Logger
import config.ClientSettings
import conn.TenhouTcpConnection
import conn.msgs.{ActionRequest, ConnMsgs, ConnReady}
import utils.MessageParseUtils

import scala.concurrent.duration.Duration

class TenhouClient(val index: Int = 0, val msgHelper: MsgHelper) extends Actor{
  //  private[this] val log = Logging(context.system, this)
  private[this] val log = Logger("TenhouClient" + index)

  //  val userName: String = TestUserName
  //  val userName: String = TenhouUserName
  val userName: String = ClientSettings.UserName
  val serverIp: String = ClientSettings.ServerIP
  val port: Int = ClientSettings.ServerPort
  val lnLimit: Int = ClientSettings.LNLimit

  var reachDropTile: Int = -1

  private[this] var mdp: ActorRef = null
  var tourEnd: Boolean = true
  val toBeInit: Boolean = false

  val props = Props(new TenhouTcpConnection(new InetSocketAddress(serverIp, port), this.self, index))
  private[client] val conn = context.actorOf(props, "tcpconnection" + index)


  import context.dispatcher
  context.system.scheduler.schedule(Duration.create(1, TimeUnit.SECONDS), Duration.create(ClientSettings.KASnap, TimeUnit.SECONDS), conn, msgHelper.getKAMsg())

  log.info("Tenhou client created")

  def scheduleGameEnd(): Unit = {
    context.system.scheduler.scheduleOnce(Duration.create(ClientSettings.GameEndWaitTime, TimeUnit.SECONDS), self, ConnMsgs.SendGameEndReply)
  }


  def dummy(x: Any): Unit = {
    x match {
      case msg: String if msg.contains("ISCLIENTOK?") =>
        mdp = sender()
      case x: AnyRef =>
        log.info("Received unknown message " + x)
    }
  }

  private var mdpReady: Boolean = false
  private var connReady: Boolean = false

  private def ready(): Unit = {
    if (mdpReady && connReady) {
      lnCount = 0
      mdpReady = false
      connReady = false

      conn ! msgHelper.getHeloMsg(userName)
      log.info("Sent hello to server")
      context.become(auth(sender()))
    }
  }

  def receive = {
    case msg: String if msg.equals(ConnMsgs.StartConnection) =>
      conn ! msg
      mdp = sender()
      mdp ! ConnMsgs.StartConnection
    case m: ConnReady =>
      connReady = true
      log.info("Connection ready")
      ready()
    case request: ActionRequest if request.action == ConnMsgs.ResetAction =>
      mdp = sender()
      mdpReady = true
      log.info("mdp ready")
      ready()
    case x: AnyRef => dummy(x)
  }

  def slowDown(): Unit = {
    Thread.sleep(200)
  }

  def auth(mx : ActorRef): Receive = {
    case msg: String if msg.contains("HELO") =>
      conn ! msgHelper.getHeloReply(msg)
      slowDown()
      conn ! msgHelper.getPxrMsg()
    case msg: String if msg.contains("LN") =>
      conn ! msgHelper.getKAMsg()
      slowDown()
      conn ! msgHelper.getJoinMsg()
      context.become(join(mx))
    case msg: String if msg.contains("SAIKAI") =>
      context.become(inGame(mx))
    case msg: String if msg.equals(ConnMsgs.ClosedConnection) =>
      gameEnd = true
      tourEnd = true
      conn ! msgHelper.getByeMsg()
      scheduleGameEnd()
      context.become(gameEnd(mx))
    case x: Any => dummy("auth: " + x)
  }

  var lnCount: Int = 0
  def join(mx: ActorRef): Receive = {
    case msg: String if msg.contains("LN") =>
      lnCount += 1
      log.info("Received repeated LN " + lnCount)
      if (lnCount > lnLimit) {
        lnCount = 0
        log.error("-----------------------> Failed in multiple LN try")

        conn ! ConnMsgs.CloseConnection
        context.become(retryInit(mx))
      }
      conn ! msgHelper.getKAMsg()
    case msg: String if msg.contains("REJOIN") =>
      conn ! msgHelper.getRejoinMsg(msg)
    case msg: String if msg.contains("GO") =>
      tourEnd = false
      //TODO: Parse i am from GO message
      conn ! msgHelper.getGokMsg()
      slowDown()
      conn ! msgHelper.getNextReadyMsg()
      context.become(inGame(mx))
    case msg: String if msg.contains("UN") =>
      log.info("Received UN")
    case msg: String if msg.contains("TAIKYOKU") =>
      msgHelper.parseTaikyokuMsg(msg)
      context.become(inGame(mx))
    case x: Any => dummy("join " + x)
  }

  def init(mx: ActorRef): Receive = {
    case request: ActionRequest =>
      mdp = sender()
      conn ! msgHelper.getNextReadyMsg()
      context.become(inGame(mx))
    case msg: String if msg.contains("PROF") => // Seemed impossible
      log.error("Impossible case? ")
      tourEnd = true
      conn ! msgHelper.getByeMsg()
      scheduleGameEnd()
      context.become(gameEnd(mx))
    //      slowDown()
    //      conn ! MessageParseUtils.CloseConnection
    //      context.become(terminated(mx))

    case x: AnyRef => dummy("init " + x)
  }

  protected def inGame(mx: ActorRef): Receive = {
    case msg: String if MessageParseUtils.isGameMsg(msg) =>
      log.info("Received message to be processed: " + msg)
      if (msgHelper.requiresAction(msg)) {
        msg match {
          case s if msgHelper.isTerminalMsg(s) => // Agari, Ryu
            gameReward += msgHelper.parseGameEndMsg(msg)
            scheduleGameEnd()
            context.become(gameEnd(mx))
          case msg: String if msg.contains("TAIKYOKU") =>
            msgHelper.parseTaikyokuMsg(msg)
          case s if s.startsWith("<T") && !s.contains("TAIKYOKU") => // <T
            if (msgHelper.isRonIndicator(s)) { // <T t="16, etc">
              conn ! msgHelper.genRonMsg(msg)
            } else if (msgHelper.isReachIndicator(msg)) {  // <T t="32">
              log.error("-----------------------------------------> Received reach indicator")
              mdp ! msgHelper.genReachResponse(s)
              context.become(inAction(mx))
            } else {
              mdp ! msgHelper.generateMyActionReply(msg)
              context.become(inAction(mx))
            }
          case s if s.contains(" t=") =>
            if (msgHelper.isRonIndicator(s)) {
              conn ! msgHelper.genRonMsg(s)
            }else {
              mdp.tell(msgHelper.generatePeerActionReply(msg), this.self) //TODO v4
              context.become(inAction(mx))
            }
          case s if s.contains("<N") && MessageParseUtils.getWhoFromN(s) == msgHelper.iam() => // the second step of steal
            msgHelper.parseNoReplyMsg(s)
            mdp.tell(msgHelper.genDropActionReply(), this.self) //TODO v4, tryValue cleaned when steal action sent
            context.become(inAction(mx))
          case _ =>
            log.error("Received unexpected message in requireAction " + msg)
        }
      }else {
        if (MessageParseUtils.isMyReachIndicator(msg)) {
          conn ! MessageParseUtils.getLogMsg("D p=\"" + reachDropTile + "\"")
        }

        msgHelper.parseNoReplyMsg(msg)
        if (msgHelper.reached()) {
          context.become(inReach(mx))
        }
      }
    case msg: String if msg.contains("PROF") =>
      tourEnd = true
      scheduleGameEnd()
      context.become(gameEnd(mx))
    //      context.become(terminated(mx))
    case msg: String if msg.equals(MessageParseUtils.ClosedConnection) =>  //TODO
      tourEnd = true
      mdp ! msgHelper.genAbortResponse()
      context.become(receive)
    case x: Any => dummy("inGame " + x)
  }

  protected def inReach(mx: ActorRef): Receive = {
    case msg: String if msgHelper.isRonIndicator(msg) =>
      conn ! msgHelper.genRonMsg(msg)
    case msg: String if msgHelper.isTerminalMsg(msg) =>
      log.debug("------------------------------> I am in reach")
      gameReward += msgHelper.parseGameEndMsg(msg)
      scheduleGameEnd()
      context.become(gameEnd(mx))
    case msg: String if msg.startsWith("<T") =>
      if (msg.contains(" t=")) {
        conn ! msgHelper.genRonMsg(msg)
        context.become(inGame(mx))
      }else {
        conn ! msgHelper.genDropAfterReach(msg)
      }
    case msg: String if MessageParseUtils.isGameMsg(msg) =>
      //      log.info("Received message after reach " + msg)
      msgHelper.parseNoReplyMsg(msg)
    case msg: String if msg.equals(MessageParseUtils.ClosedConnection) =>
      tourEnd = true
      mdp ! msgHelper.genAbortResponse()
      context.become(receive)
    case x: Any => dummy("inReach " + x)
  }

  protected  def inAction(mx: ActorRef): Receive = {
    case request: ActionRequest => {
      mdp = sender()
      request.action match {
        case ClientSettings.NOOPWoAccept =>
          conn ! msgHelper.parseNoopAction()
          context.become(inGame(mx))
        case action if msgHelper.isDropAction(action) =>
          log.info("----------------------------> In drop action ")
          if (msgHelper.isInReachAction()) {
            log.info("--------------------------_> In reach action")
            val msgs = msgHelper.genReachMsg(action).split("\\|").map(_.trim)
            reachDropTile = MessageParseUtils.dropTileFromMsg(msgs(0))
            //            log.info(msgs.mkString(","))
            msgs.foreach(msg => {
              conn ! msg
              //              slowDown()
            })
            context.become(inGame(mx))
          } else if (msgHelper.isStealResponse()) { //Decide not to steal
            log.info("-------------------------> In steal action")
            conn ! msgHelper.parseNoopAction()
            context.become(inGame(mx))
          } else {
            log.info("------------------------------> Pure drop")
            conn ! msgHelper.genDropOnlyMsg(action)
            context.become(inGame(mx))
          }
        case action if action == ClientSettings.REACHWoAccept =>
          log.info("MDP decides to reach")
          mdp.tell(msgHelper.genReachDropActionReply(), this.self)
        case action if msgHelper.isStealAction(action) =>
          conn ! msgHelper.genStealActionMsg(action)
          context.become(inGame(mx))
      }
    }
    case msg: String if msg.equals(MessageParseUtils.ClosedConnection) =>
      tourEnd = true
      context.become(aborted(mx))
    case x: Any => dummy("inAction " + x)
  }

  protected  def aborted(mx: ActorRef): Receive = {
    case request: ActionRequest =>
      log.info("Connection closed in aborted")
      Thread.sleep(30000)
      mdp = sender()
      mdp ! msgHelper.genAbortResponse()
      context.become(receive)
    case x: Any => dummy("aborted" + x)
  }

  protected  def retryInit(mx: ActorRef): Receive = {
    case msg: String if msg.equals(MessageParseUtils.ClosedConnection) =>
      log.info("Connection closed in retryInit")
      tourEnd = true
      mdpReady = true
      conn ! MessageParseUtils.StartConnection
      context.become(receive)
    case x: Any => dummy("close " + x)
  }

  var gameEnd: Boolean = false
  var gameReward: Int = 0

  private def clearGameEnd(): Unit = {
    gameEnd = false
    tourEnd = false
    gameReward = 0
  }

  protected def gameEnd(mx: ActorRef):Receive = {
    case msg: String if msg.contains("PROF") =>
      tourEnd = true
      conn ! msgHelper.getByeMsg()
    case msg: String if msgHelper.isTerminalMsg(msg) =>
      gameEnd = true
      gameReward += msgHelper.parseGameEndMsg(msg)
    case msg: String if msg.equals(MessageParseUtils.SendGameEndReply) =>
      log.info("To send game end reply to mdp")
      val endMsg = msgHelper.genGameEndReply(gameReward, tourEnd)
      if (tourEnd) {
        clearGameEnd()

        conn ! MessageParseUtils.CloseConnection
        mdp ! endMsg
        //        context.become(terminated(mx))
      }else {
        clearGameEnd()
        mdp ! endMsg
        //        conn ! msgHelper.getNextReadyMsg()
        context.become(init(mx))
      }
    //TODO: send reply and goto init or teminated
    case msg: String if msg.equals(MessageParseUtils.ClosedConnection) =>
      log.info("Connection closed ")
      context.become(receive)
    case msg: String if msg.equals(MessageParseUtils.StartConnection) =>  //received in advance
      conn ! msg
      mdp = sender()
      mdp ! MessageParseUtils.StartConnection
    case x: Any => dummy("gameEnd " + x)
  }

}
