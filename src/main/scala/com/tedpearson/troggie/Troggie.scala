package com.tedpearson.troggie

import java.io.BufferedWriter
import java.io.File
import java.io.FileInputStream
import java.io.FileOutputStream
import java.io.FileWriter
import java.sql.DriverManager
import java.util.Properties
import org.jibble.pircbot.PircBot
import akka.actor._
import akka.routing.BroadcastRouter
import akka.pattern.ask
import scala.compat.Platform._
import org.joda.time.format.PeriodFormat
import org.joda.time.Period
import org.scalaquery.session.Database
import org.joda.time.PeriodType
import org.joda.time.format.PeriodFormatterBuilder
import akka.dispatch.Future
import akka.util.Timeout
import akka.dispatch.Await

class Troggie(network: String) extends PircBot with Actor {
  var (sentMsgs, recdMsgs) = (0L, 0L)
  val version = "3.0b1"
  import Troggie._ => t
  val logfile = new File(properties.getProperty("logfile","troggie.log"))
  logfile.createNewFile
  val log = new BufferedWriter(new FileWriter(logfile, true))
  setVerbose(true)
  val nick = properties.getProperty("nick")
  val Ignore = Option(properties.getProperty("ignore")) map { _.replace(",","|").replace("*",".*?").r }
  setName(nick)
  setLogin(nick)
  setEncoding("UTF-8")
  val port = Integer.parseInt(properties.getProperty("port", "6667"))
  val database = properties.getProperty("database")
  val session = Database.forURL("jdbc:sqlite:%s" format database, driver = "org.sqlite.JDBC").createSession()
  val plugins = loadPlugins
  val router = context.actorOf(Props[Plugin].withRouter(BroadcastRouter(routees = plugins)), name="router")
  router ! SelfNickChange(nick)
  implicit val system = context.system
  
  // don't connect until actor is started
  // otherwise we try to start sending messages before akka is ready
  override def preStart() {
    connect(network, port, properties.getProperty("server_password"))
    joinChannels
  }
  
  // disconnect when akka is shut down so the program will die appropriately
  override def postStop() {
    dispose
    session.close
  }

  private def joinChannels {
    for(c <- properties.getProperty(network + "_channels").split(",")) {
      joinChannel(c)
    }
  }
  
  private def loadPlugins: Seq[ActorRef] = {
    val conf = new PluginConf(properties, session)
    for(plugin <- properties.getProperty("plugins").split(",") if pluginDefined(plugin))
      yield context.actorOf(Props(Class.forName(plugin).getConstructor(
          classOf[PluginConf]).newInstance(conf).asInstanceOf[Plugin]), name=plugin)
  }
  
  private def pluginDefined(plugin: String) = {
    try {
      Class.forName(plugin)
      true
    } catch {
      case e: Exception => e.printStackTrace
          false
    }
  }
  
  def receive = {
    case m: SendMessage => {
      sentMsgs += 1
      sendMessage(m.target, m.msg)
    }
    case m: SendAction => {
      sentMsgs += 1
      sendAction(m.target, m.msg)
    }
    case _ => println("unknown message")
  }
  
  val StatusRE = """(?i)^\s*status\s*\?*$""".r
  val launchTime = currentTime
  import akka.util.duration._
  implicit val timeout = Timeout(500 milliseconds)
  def doStatus(from: String, msg: String) {
    if(StatusRE.pattern.matcher(msg).matches) {
      val period = Utils.formatSince(launchTime, currentTime)
      val futures = for(p <- plugins) yield (p ? GetStatus()).mapTo[Status]
      Future {
        val fList = Future.sequence(futures.map(_ recover { case _ => Status("") }))
        val result = Await.result(fList, Timeout(1 second).duration).filter(_.status != "")
        val status = for (s <- result) yield s.status
        self ! SendMessage(from, "Uptime: %s. Messages in/out: %d/%d. %s"
          format(period, recdMsgs, sentMsgs, status.mkString(""," ","")))
      }
    }
  }
  
  def send(msg: IrcMessage) {
    Ignore match {
      case Some(r) => msg match {
        case m: Message => "%s!%s@%s".format(m.sender, m.login, m.host) match {
          case `r`() => return
          case _ =>
        }
        case _ =>
      }
      case _ =>
    }
    router ! msg
  }

  override def log(line: String) {
    if(!line.contains("PING :") & !line.contains("PONG :")) {
      val toLog = currentTime + " " + line
          log.write(toLog)
          log.newLine()
          log.flush
          Console.println(toLog)
    }
  }
  
  override def onVersion(sourceNick: String, sourceLogin: String, sourceHostname: String, target: String) {
    sendRawLine("NOTICE %s :\u0001VERSION Troggie %s - https://github.com/tedpearson/Troggie (A %s)\u0001"
        format(sourceNick, version, getVersion()));
  }
  
  override def onDisconnect() {
    while(!isConnected()) {
      try {
        reconnect
        joinChannels
      } catch {
        case _ => Thread sleep 5000
      }
    }
  }
  
  override def onMessage(channel: String, sender: String, login: String, host: String, msg: String) {
    recdMsgs += 1
    doStatus(channel, msg)
    send(PublicMessage(channel, sender, login, host, msg))
  }
  
  override def onPrivateMessage(sender: String, login: String, host: String, msg: String) {
    recdMsgs += 1
    doStatus(sender, msg)
    send(PrivateMessage(sender, login, host, msg))
  }
  
  override def onAction(sender: String, login: String, host: String, target: String, action: String) {
    send(Action(sender, login, host, target, action))
  }
  
  override def onDeop(channel: String, sender: String, login: String, host: String, rcpt: String) {
    send(Deop(channel, sender, login, host, rcpt))
  }
  
  override def onDeVoice(channel: String, sender: String, login: String, host: String, rcpt: String) {
    send(DeVoice(channel, sender, login, host, rcpt))
  }
  
  override def onJoin(channel: String, sender: String, login: String, hostname: String) {
    sender match {
      // backticks required with lowercase variable here
      // otherwise it will assume we're declaring a new variable to bind on match
      case `nick` => send(SelfJoin(channel))
      case _ => send(Join(channel, sender, login, hostname))
    }
  }
  
  override def onKick(channel: String, sender: String, login: String, host: String, rcpt: String, msg: String) {
    rcpt match {
      case `nick` => {
        // TODO make async
        Thread sleep 1000
        joinChannel(channel)
      }
      case _ =>
    }
    send(Kick(channel, sender, login, host, rcpt, msg))
  }
  
  override def onNickChange(oldNick: String, login: String, host: String, newNick: String) {
    oldNick match {
      case `nick` => send(SelfNickChange(newNick))
      case _ => send(NickChange(oldNick, login, host, newNick))
    }
  }
  
  override def onNotice(sender: String, login: String, host: String, target: String, msg: String) {
    send(Notice(sender, login, host, target, msg))
  }
  
  override def onOp(channel: String, sender: String, login: String, host: String, rcpt: String) {
    send(Op(channel, sender, login, host, rcpt))
  }
  
  override def onPart(channel: String, sender: String, login: String, host: String) {
    send(Part(channel, sender, login, host))
  }
  
  override def onQuit(sender: String, login: String, host: String, msg: String) {
    send(Quit(sender, login, host, msg))
  }
  
  override def onTopic(channel: String, topic: String, setBy: String, date: Long, change: Boolean) {
    send(Topic(channel, topic, setBy, date, change))
  }
  
  override def onVoice(channel: String, sender: String, login: String, host: String, rcpt: String) {
    send(Voice(channel, sender, login, host, rcpt))
  }
}

object Troggie extends App {
  var confFile = "troggie.conf"
  val properties = new Properties
  if(args.length != 0) {
    confFile = args(0)
  }
  val propFile = new File(confFile)
  if(propFile.createNewFile()) {
    properties.setProperty("nick", "troggi");
    properties.setProperty("networks", "irc.freenode.net");
    properties.setProperty("irc.freenode.net_channels", "#troggie");
    properties.setProperty("plugins", "com.tedpearson.troggie.Factoid"
        + ",com.tedpearson.troggie.Seen"
        + ",com.tedpearson.troggie.GoogleSearch"
        + ",com.tedpearson.troggie.UrlTitle"
        + ",com.tedpearson.troggie.GoogleTranslate");
    properties.setProperty("database","troggie-db");
    properties.store(new FileOutputStream(propFile), "")
  }
  properties.load(new FileInputStream(propFile))
  val networks = properties.getProperty("networks").split(",")
  
  val system = ActorSystem("Troggie")
  
  for(n <- networks) {
    system.actorOf(Props(new Troggie(n)), name=n)
  }
  
  Thread.currentThread.getName match {
    case "main" =>
    case _ => {
      pressKeyToStop
    }
  }

  def pressKeyToStop {
    while(System.in.available() <= 0) {
      try { Thread.sleep(1000) } catch { case _: InterruptedException => () }
    }
    system.shutdown()
  }
}

object Utils {
  val p = new PeriodFormatterBuilder()
  .appendYears()
  .appendSuffix(" year"," years")
  .appendSeparator(", ")
  .appendMonths()
  .appendSuffix(" month"," months")
  .appendSeparator(", ")
  .appendWeeks()
  .appendSuffix(" week"," weeks")
  .appendSeparator(", ")
  .appendDays()
  .appendSuffix(" day"," days")
  .appendSeparator(", ")
  .appendHours()
  .appendSuffix(" hour"," hours")
  .appendSeparator(", ")
  .appendMinutes()
  .appendSuffix(" minute"," minutes")
  .appendSeparator(", ")
  .printZeroAlways()
  .appendSeconds()
  .appendSuffix(" second"," seconds")
  .toFormatter()
  def formatSince(start: Long, end: Long) = {
    p.print(new Period(start, end))
  }
}